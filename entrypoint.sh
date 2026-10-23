#!/bin/bash
set -Eeuo pipefail

auth_enabled=${AUTH_ENABLED:-false}
auth_enabled=${auth_enabled,,}

case "$auth_enabled" in
  false | "")
    exec /usr/bin/shiny-server
    ;;
  true) ;;
  *)
    echo "error: AUTH_ENABLED must be true or false (got: $auth_enabled)" >&2
    exit 1
    ;;
esac

missing=()
require_line() {
  if [[ -z $2 ]]; then
    missing+=("$1")
  elif [[ $2 == *$'\n'* ]]; then
    echo "error: $1 must be a single line" >&2
    exit 1
  fi
}

require_line OAUTH2_PROXY_CLIENT_ID "${OAUTH2_PROXY_CLIENT_ID:-}"
require_line OAUTH2_PROXY_CLIENT_SECRET "${OAUTH2_PROXY_CLIENT_SECRET:-}"
require_line OAUTH2_PROXY_OIDC_ISSUER_URL "${OAUTH2_PROXY_OIDC_ISSUER_URL:-}"
require_line OAUTH2_PROXY_REDIRECT_URL "${OAUTH2_PROXY_REDIRECT_URL:-}"
require_line OAUTH2_PROXY_COOKIE_SECRET "${OAUTH2_PROXY_COOKIE_SECRET:-}"
if ((${#missing[@]})); then
  echo "error: AUTH_ENABLED=true requires: ${missing[*]}" >&2
  exit 1
fi

cookie_secure=${OAUTH2_PROXY_COOKIE_SECURE:-true}
cookie_secure=${cookie_secure,,}
case "$cookie_secure" in
  true | false) ;;
  *)
    echo "error: OAUTH2_PROXY_COOKIE_SECURE must be true or false" >&2
    exit 1
    ;;
esac

issuer=$OAUTH2_PROXY_OIDC_ISSUER_URL
redirect=$OAUTH2_PROXY_REDIRECT_URL

case "$cookie_secure" in
  true)
    case "$redirect" in
      https://*/oauth2/callback) ;;
      http://*)
        echo "error: OAUTH2_PROXY_COOKIE_SECURE=false is required for an http redirect URL (local testing only)" >&2
        exit 1
        ;;
      *)
        echo "error: OAUTH2_PROXY_REDIRECT_URL must be an absolute https URL ending in /oauth2/callback" >&2
        exit 1
        ;;
    esac
    case "$issuer" in
      https://*) ;;
      *)
        echo "error: OAUTH2_PROXY_OIDC_ISSUER_URL must use https" >&2
        exit 1
        ;;
    esac
    cookie_name=__Host-gc_auth
    ;;
  false)
    case "$redirect" in
      https://*)
        echo "error: OAUTH2_PROXY_COOKIE_SECURE must be true when OAUTH2_PROXY_REDIRECT_URL is https" >&2
        exit 1
        ;;
    esac
    case "$issuer" in
      https://* | http://*) ;;
      *)
        echo "error: OAUTH2_PROXY_OIDC_ISSUER_URL must be an absolute http(s) URL" >&2
        exit 1
        ;;
    esac
    case "$redirect" in
      http://*/oauth2/callback) ;;
      *)
        echo "error: OAUTH2_PROXY_REDIRECT_URL must be an absolute URL ending in /oauth2/callback" >&2
        exit 1
        ;;
    esac
    cookie_name=_gc_auth
    ;;
esac

case "$issuer$redirect" in
  *'?'* | *'#'* | *' '*)
    echo "error: OIDC issuer and redirect URLs must not contain a query, fragment, or space" >&2
    exit 1
    ;;
esac

app_host=${redirect#*://}
app_host=${app_host%%/*}
issuer_host=${issuer#*://}
issuer_host=${issuer_host%%/*}

proxied_config=/tmp/shiny-server-proxied.conf
sed 's/listen 3838;/listen 3839 127.0.0.1;/' \
  /etc/shiny-server/shiny-server.conf > "$proxied_config"
if ! grep -q 'listen 3839 127.0.0.1;' "$proxied_config"; then
  echo "error: could not configure Shiny Server for the private upstream" >&2
  exit 1
fi

/usr/bin/shiny-server "$proxied_config" &
shiny_pid=$!

# Drop inherited proxy settings so mounted paths cannot be exempted from auth.
env -i \
  SSL_CERT_FILE="${SSL_CERT_FILE:-/etc/ssl/certs/ca-certificates.crt}" \
  /usr/local/bin/oauth2-proxy \
  --provider=oidc \
  --provider-display-name=Auth0 \
  --oidc-issuer-url="$issuer" \
  --client-id="$OAUTH2_PROXY_CLIENT_ID" \
  --client-secret="$OAUTH2_PROXY_CLIENT_SECRET" \
  --redirect-url="$redirect" \
  --scope="openid email profile" \
  --skip-claims-from-profile-url=true \
  --insecure-oidc-allow-unverified-email=true \
  --oidc-groups-claim=urn:gc:roles \
  --allowed-group=Member \
  --allowed-group=Admin \
  --code-challenge-method=S256 \
  --email-domain="*" \
  --cookie-secret="$OAUTH2_PROXY_COOKIE_SECRET" \
  --cookie-secure="$cookie_secure" \
  --cookie-httponly=true \
  --cookie-samesite=lax \
  --cookie-name="$cookie_name" \
  --session-cookie-minimal=true \
  --skip-provider-button=true \
  --reverse-proxy=true \
  --trusted-proxy-ip=127.0.0.1 \
  --trusted-proxy-ip=10.0.0.0/8 \
  --trusted-proxy-ip=172.16.0.0/12 \
  --trusted-proxy-ip=192.168.0.0/16 \
  --trusted-proxy-ip=::1 \
  --trusted-proxy-ip=fc00::/7 \
  --whitelist-domain="$app_host" \
  --whitelist-domain="$issuer_host" \
  --http-address=0.0.0.0:3838 \
  --upstream=http://127.0.0.1:3839/ \
  --silence-ping-logging=true &
proxy_pid=$!

cleanup() {
  kill "$proxy_pid" "$shiny_pid" 2>/dev/null || true
  wait "$proxy_pid" "$shiny_pid" 2>/dev/null || true
}
trap 'exit 143' TERM INT
trap cleanup EXIT

if wait -n "$shiny_pid" "$proxy_pid"; then
  status=0
else
  status=$?
fi

if kill -0 "$shiny_pid" 2>/dev/null; then
  echo "error: oauth2-proxy exited unexpectedly" >&2
else
  echo "error: Shiny Server exited unexpectedly" >&2
fi
exit "$status"
