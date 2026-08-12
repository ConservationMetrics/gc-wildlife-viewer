#' Load and Standardize Camera Trap Metadata
#'
#' Reads a metadata CSV file produced by camera trap or TimeLapse-style
#' workflows, standardizes column names, constructs image paths, and
#' parses date-time fields. This function is intended as a preprocessing
#' step for downstream image browsing, mapping, or analysis workflows.
#'
#' The function expects a CSV containing at minimum a \code{camera},
#' \code{relative_path}, and \code{file} column. Column names are cleaned
#' using \code{janitor::clean_names()}. A \code{site_name} column is derived
#' from \code{camera}. Image paths are constructed in both flattened and
#' hierarchical forms to support different export conventions.
#'
#' If latitude and longitude columns are missing, placeholder values are
#' generated. These are intended for development and demonstration only
#' and should be removed or replaced when working with real spatial data.
#'
#' @param csv_path Character scalar. Path to a metadata CSV file on disk.
#' @param deployment_data_path Character scalar. Path to a deployment_data CSV
#'   file on disk. Must follow the CMI template.
#' @param rel_path_parts assumes the relative path captures important
#'   information that can be pared into its folder structure.  column names to
#'   be created by splitting the relative path on "/". Default = c("deployment",
#'   "region", "camera","survey_location")
#' @param verbose print informative messages
#' @param site_name_cols names of columns to concatenate into the "site_name"
#'   used for mapping.  These should make up the unique key identifying a
#'   site/name
#'
#' @return A data.frame containing the cleaned and augmented metadata.
#'   Additional columns include:
#'   \itemize{
#'     \item \code{site_name}: Site identifier derived from \code{camera}
#'     \item \code{image_path_flat}: Flattened image path (dots instead of separators)
#'     \item \code{image_path}: Hierarchical image path with forward slashes
#'     \item \code{thumb_path}: Alias of \code{image_path} for thumbnail use
#'     \item \code{date_time}: Parsed POSIXct timestamp (UTC), if present
#'   }
#'
#' @details
#' Date-time parsing attempts several common formats, including ISO 8601.
#' If no \code{date_time} column is present, the returned column will contain
#' \code{NA} values.
#'
#' The function will stop with an error if the CSV file does not exist.
#'
#' @examples
#' \dontrun{
#' meta <- load_metadata("data/metadata.csv")
#' head(meta)
#' }
#'
#' @importFrom stats rnorm
#' @importFrom tidyr separate_wider_delim
#' @importFrom utils read.csv
#' @importFrom janitor clean_names
#' @export

#' Read a CSV as UTF-8, tolerating TimeLapse/Windows encodings
#'
#' TimeLapse exports on Western Windows are often Windows-1252. Reading those
#' as UTF-8 leaves invalid byte sequences that break Shiny/`jsonlite` (and
#' show up as \code{�}). Characters like \code{ë} are fine once decoded to UTF-8.
#'
#' For future reference, a different approach to use `readr` R package is documented here: https://github.com/ConservationMetrics/gc-wildlife-viewer/pull/29#pullrequestreview-4920922074
#' @param path Character scalar. Path to a CSV file.
#' @param verbose Logical. If \code{TRUE}, report the encoding used.
#' @return A data.frame with character columns marked UTF-8.
#' @keywords internal
read_csv_utf8 <- function(path, verbose = FALSE) {
    raw <- readBin(path, what = "raw", n = file.info(path)$size)

    detected <- tryCatch({
        hits <- stringi::stri_enc_detect(raw)[[1]]
        hits$Encoding[hits$Confidence >= 0.2]
    }, error = function(e) character())

    # UTF-8 first; Windows-1252 next (TimeLapse/Excel on Western Windows)
    candidates <- unique(c("UTF-8", "UTF-8-BOM", detected, "windows-1252", "ISO-8859-1"))

    for (enc in candidates) {
        if (enc %in% c("UTF-8", "UTF-8-BOM") && !isTRUE(stringi::stri_enc_isutf8(raw))) {
            next
        }

        text <- tryCatch(
            stringi::stri_encode(raw, from = enc, to = "UTF-8"),
            error = function(e) NULL
        )
        if (is.null(text) || !nzchar(text)) next

        df <- tryCatch(
            utils::read.csv(
                textConnection(text, encoding = "UTF-8"),
                stringsAsFactors = FALSE,
                check.names = FALSE
            ),
            error = function(e) NULL
        )
        if (is.null(df) || ncol(df) < 1) next

        chr <- vapply(df, is.character, logical(1))
        df[chr] <- lapply(df[chr], function(col) {
            col <- stringi::stri_enc_toutf8(col, validate = TRUE)
            Encoding(col) <- "UTF-8"
            col
        })

        if (verbose) message("  Decoded ", basename(path), " as ", enc)
        return(df)
    }

    stop("Could not decode CSV as UTF-8 (tried: ", paste(candidates, collapse = ", "), "): ", path)
}

load_metadata <- function(csv_path,
                          deployment_data_path,
                          rel_path_parts = c("deployment", "region", "camera","location_name"), 
                          site_name_cols,
                          verbose = TRUE) {
    
    if (!file.exists(csv_path)) {
        stop("Metadata CSV not found: ", csv_path)
    }
    
    if (verbose) message("Loading metadata from: ", csv_path)
    
    meta <- read_csv_utf8(csv_path, verbose = verbose) %>%
        janitor::clean_names() %>%
        # NOTE: this is custom for a specific user's data
        # We need to come up with the supported method for this.
        {
            if (!"camera" %in% names(.) && "relative_path" %in% names(.)) {
                tidyr::separate_wider_delim(
                    .,
                    cols = relative_path,
                    delim = "\\",
                    names = rel_path_parts,
                    cols_remove = FALSE
                )
            } else {
                .
            }
        }

    if ("camera" %in% names(meta)) {
        meta$camera <- gsub(".*\\\\", "", meta$camera)
    }
    if ("location_name" %in% names(meta)) {
        meta$location_name <- tolower(meta$location_name)
    }
    if ("date_time" %in% names(meta)) {
        meta$datetime <- ymd_hms(meta$date_time, tz = "UTC")
    } else {
        meta$datetime <- as.POSIXct(NA)
    }

    if (file.exists(deployment_data_path)) {
        if (verbose) message("Found deployment data at: ", deployment_data_path, " — joining with metadata")
        deploy <- read.csv(deployment_data_path, stringsAsFactors = FALSE) %>%
            janitor::clean_names()

        if (all(c("deployment_date", "deployment_time") %in% names(deploy))) {
            deploy$deployment_datetime <- mdy_hms(
                paste(deploy$deployment_date, deploy$deployment_time),
                tz = "UTC"
            )
        }
        if (all(c("retrieval_date", "retrieval_time") %in% names(deploy))) {
            deploy$retrieval_datetime <- mdy_hms(
                paste(deploy$retrieval_date, deploy$retrieval_time),
                tz = "UTC"
            )
        }
        if ("location_name" %in% names(deploy)) {
            deploy$location_name <- tolower(deploy$location_name)
        }
        if (all(site_name_cols %in% names(deploy))) {
            deploy$site_name <- do.call(paste, c(deploy[site_name_cols], sep = " - "))
        }

        join_ok <- all(c("location_name", "region", "camera", "datetime") %in% names(meta)) &&
            all(c("location_name", "region", "camera_name",
                  "deployment_datetime", "retrieval_datetime") %in% names(deploy))

        if (join_ok) {
            meta <- meta %>%
                left_join(deploy,
                          by = join_by(location_name, region, camera == camera_name,
                                       between(datetime, deployment_datetime, retrieval_datetime)))
        } else if (verbose) {
            message("Skipping deployment join — missing required columns in metadata and/or deployment data")
        }
    }

    if (!"site_name" %in% names(meta) || all(is.na(meta$site_name))) {
        meta$site_name <- if ("camera" %in% names(meta)) meta$camera else "unknown"
    }
    if (!"latitude" %in% names(meta) || !"longitude" %in% names(meta)) {
        if (verbose) message("Missing lat/lon — spoofing coordinates for map display")
        if (!"latitude" %in% names(meta)) meta$latitude <- 0.9 + rnorm(nrow(meta), mean = 0.01, sd = 0.1)
        if (!"longitude" %in% names(meta)) meta$longitude <- 34.6 + rnorm(nrow(meta), mean = 0.01, sd = 0.1)
    }
    # Build flattened path (for TimeLapse exports with backslashes)
    meta$image_path_flat <- file.path(
        gsub("\\", ".", paste0(meta$relative_path, ".", meta$file), fixed = TRUE)
    )
    # Build proper hierarchical paths
    meta$image_path <- gsub("\\\\", "/", file.path(meta$relative_path, meta$file))
    meta$thumb_path <- gsub("\\\\", "/", file.path(meta$relative_path, meta$file))
    
    # Adjust thumbnail paths for videos (change extension to .jpg)
    meta$is_video <- sapply(meta$image_path, is_video)
    
    meta$thumb_path <- ifelse(
        meta$is_video,
        sub("\\.[^.]*$", ".jpg", meta$thumb_path),
        meta$thumb_path
    )
    
    # Date-time parsing
    if (verbose) message("Parsing date-time values...")
    if("date_time" %in% names(meta)){
        meta$date_time <- as.POSIXct(meta$date_time, tz = "UTC",
                                     tryFormats = c("%Y-%m-%dT%H:%M:%OS",
                                                    "%Y-%m-%d %H:%M:%OS",
                                                    "%Y-%m-%d"))
    } else {
        meta$date_time <- NA
    }
    
    if (verbose) {
        message("Metadata loading complete! Loaded ", nrow(meta), " records from ", 
                length(unique(meta$site_name)), " sites.")
    }
    
    return(meta)
}

#' Reconstruct Hierarchical Image Paths from Flattened Filenames
#'
#' Converts flattened image filenames into a hierarchical directory
#' structure on disk. This function is intended for use with metadata
#' produced by \code{load_metadata()}, where image paths have been
#' flattened (e.g., for TimeLapse exports) and need to be restored to
#' their original folder layout.
#'
#' The function creates the required directory structure under
#' \code{image_dir} and renames files from their flattened locations
#' to hierarchical paths. File operations are performed in place.
#'
#' @param meta A data.frame containing image metadata. Must include
#'   \code{image_path_flat} and \code{image_path} columns.
#' @param image_dir Character scalar. Base directory containing the
#'   flattened image files and where the hierarchical structure will
#'   be created.
#' @param verbose Logical. If \code{TRUE} (default), progress and summary
#'   messages are printed to the console.
#'
#' @details
#' For each row in \code{meta}, the function attempts to rename a file
#' from \code{file.path(image_dir, image_path_flat)} to
#' \code{file.path(image_dir, image_path)}. Directories are created
#' recursively as needed. Files that already exist at the destination
#' path are skipped.
#'
#' This function has side effects on the filesystem and does not return
#' a value. It is recommended to run it on a copy of your data during
#' testing.
#'
#' @return Invisibly returns \code{NULL}. The primary effect is the
#'   reorganization of files on disk.
#'
#' @examples
#' \dontrun{
#' meta <- load_metadata("metadata.csv")
#' unflatten_paths(meta, image_dir = "images")
#' }
#'
#' @seealso \code{\link{load_metadata}}
#'
#' @export
unflatten_paths <- function(meta, image_dir, verbose = TRUE) {
    
    if (verbose) message("Unflattening folder structure for image paths...")
    
    # Build full paths
    meta$full_path_flat <- file.path(image_dir, meta$image_path_flat)
    meta$full_path <- file.path(image_dir, meta$image_path)
    
    # Create directory structure
    unique_dirs <- unique(dirname(meta$full_path))
    lapply(unique_dirs, function(x) dir.create(x, recursive = TRUE, showWarnings = FALSE))
    
    # Rename files from flat to hierarchical structure
    n_renamed <- 0
    n_failed <- 0
    
    for (i in seq_len(nrow(meta))) {
        if (file.exists(meta$full_path_flat[i]) && !file.exists(meta$full_path[i])) {
            success <- tryCatch({
                file.rename(meta$full_path_flat[i], meta$full_path[i])
                TRUE
            }, error = function(e) FALSE)
            
            if (success) {
                n_renamed <- n_renamed + 1
            } else {
                n_failed <- n_failed + 1
            }
        }
    }
    
    if (verbose) {
        message("Unflattening complete!")
        message(sprintf("  Files renamed: %d | Failed: %d", n_renamed, n_failed))
    }
    
    # return(meta)
}

#' Determine Media Type from File Extension
#'
#' Classifies a file as image, video, or unknown based on its file extension.
#' This function is useful for camera trap workflows where media files of
#' different types are stored together and need to be processed differently.
#'
#' @param filename Character scalar. The filename or file path to classify.
#'   Can be a simple filename (e.g., \code{"image.jpg"}) or a full path
#'   (e.g., \code{"path/to/video.mp4"}). Only the file extension is used
#'   for classification.
#'
#' @return Character scalar. One of:
#'   \itemize{
#'     \item \code{"image"}: File has an image extension (jpg, jpeg, png, gif, bmp)
#'     \item \code{"video"}: File has a video extension (mp4, mov, avi, mkv, webm)
#'     \item \code{"unknown"}: File has an unrecognized extension, or filename
#'       is \code{NA} or empty
#'   }
#'
#' @details
#' The function is case-insensitive and only examines the file extension.
#' It does not verify that the file actually exists or that its content
#' matches the extension.
#'
#' Recognized extensions:
#' \itemize{
#'   \item \strong{Images}: jpg, jpeg, png, gif, bmp
#'   \item \strong{Videos}: mp4, mov, avi, mkv, webm
#' }
#'
#' @examples
#' get_media_type("path/to/camera/IMG_0001.JPG")
#' #> [1] "image"
#'
#' get_media_type("clip.MP4")
#' #> [1] "video"
#'
#' @seealso \code{\link{tools::file_ext}}
#'
#' @export
get_media_type <- function(filename) {
    if (is.na(filename) || !nzchar(filename)) return("unknown")
    ext <- tolower(tools::file_ext(filename))
    if (ext %in% c("mp4", "mov", "avi", "mkv", "webm")) {
        return("video")
    } else if (ext %in% c("jpg", "jpeg", "png", "gif", "bmp")) {
        return("image")
    } else {
        return("unknown")
    }
}
#' Check if File is a Video
#'
#' Determines whether a file is a video based on its extension.
#'
#' @param filepath Character scalar. Path to the file to check.
#'
#' @return Logical. \code{TRUE} if the file has a video extension,
#'   \code{FALSE} otherwise.
#'
#' @details
#' Supported video formats: mp4, avi, mov, mkv, mpeg, mpg, wmv, flv, webm, m4v
#'
#' @keywords internal
is_video <- function(filepath) {
    ext <- tolower(tools::file_ext(filepath))
    ext %in% c("mp4", "avi", "mov", "mkv", "mpeg", "mpg", "wmv", "flv", "webm", "m4v")
}


#' Check if ffmpeg is Available
#'
#' Tests whether ffmpeg is installed and accessible via the system PATH.
#'
#' @return Logical. \code{TRUE} if ffmpeg is available, \code{FALSE} otherwise.
#'
#' @keywords internal
check_ffmpeg <- function() {
    result <- tryCatch({
        system2("ffmpeg", args = "-version", stdout = FALSE, stderr = FALSE)
        TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    
    return(result == 0 || result == TRUE)
}


#' Create Video Thumbnail using ffmpeg
#'
#' Extracts a frame from a video file and saves it as a thumbnail image.
#'
#' @param video_path Character scalar. Path to the source video file.
#' @param thumbnail_path Character scalar. Path where the thumbnail image
#'   will be saved. Extension should be .jpg or .png.
#' @param thumb_width Numeric. Target width of the thumbnail in pixels.
#'   Height is calculated automatically to preserve aspect ratio.
#' @param seek_time Numeric. Time in seconds from which to extract the frame.
#'   Default is 1 second. Set to 0 to extract the first frame.
#'
#' @return Logical. \code{TRUE} if thumbnail was created successfully,
#'   \code{FALSE} otherwise.
#'
#' @details
#' This function uses ffmpeg to extract a single frame from a video and
#' resize it to the specified width. The ffmpeg command used is:
#' \code{ffmpeg -ss [seek_time] -i [video] -vf scale=[width]:-1 -vframes 1 [output]}
#'
#' The function automatically changes the file extension of the thumbnail
#' to .jpg regardless of the video's original extension.
#'
#' @keywords internal
create_video_thumbnail <- function(video_path, thumbnail_path, thumb_width = 300, seek_time = 1) {
    
    # Ensure thumbnail has .jpg extension
    thumbnail_path <- sub("\\.[^.]*$", ".jpg", thumbnail_path)
    
    # Build ffmpeg command
    # -ss: seek to timestamp
    # -i: input file
    # -vf scale: resize video filter (width:height, -1 = preserve aspect ratio)
    # -vframes 1: extract only 1 frame
    # -y: overwrite output file if exists
    cmd <- sprintf(
        'ffmpeg -ss %d -i "%s" -vf scale=%d:-1 -vframes 1 -y "%s"',
        seek_time,
        video_path,
        thumb_width,
        thumbnail_path
    )
    
    # Execute command, suppressing output
    result <- tryCatch({
        system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    }, error = function(e) {
        return(1)
    })
    
    # Check if thumbnail was created successfully
    success <- (result == 0 && file.exists(thumbnail_path))
    
    return(success)
}


#' Generate Thumbnails for Images and Videos
#'
#' Creates thumbnail images for camera trap photographs and videos based on
#' metadata produced by \code{load_metadata()}. Thumbnails are written to
#' disk in a directory structure that mirrors the hierarchical image paths.
#'
#' The function reads source images from \code{image_dir}, rescales them
#' to a fixed width while preserving aspect ratio, and writes the results
#' to \code{thumb_dir}. For video files, a frame is extracted using ffmpeg.
#' Existing thumbnails are skipped. Missing or unreadable files are recorded
#' as failures.
#'
#' @param meta A data.frame containing image metadata. Must include
#'   \code{image_path} and \code{thumb_path} columns.
#' @param image_dir Character scalar. Base directory containing the
#'   original image and video files.
#' @param thumb_dir Character scalar. Base directory where thumbnails
#'   will be written.
#' @param thumb_width Numeric. Target width of generated thumbnails in
#'   pixels. Aspect ratio is preserved. Default is \code{300}.
#' @param video_seek_time Numeric. For video files, the time in seconds
#'   from which to extract the thumbnail frame. Default is \code{1}.
#'   Set to \code{0} to extract the first frame.
#' @param verbose Logical. If \code{TRUE} (default), progress and summary
#'   messages are printed to the console.
#'
#' @details
#' For each row in \code{meta}, the function determines whether the file
#' is an image or video based on its extension. Images are processed using
#' the \pkg{magick} package. Videos are processed using ffmpeg, which must
#' be installed and available in the system PATH.
#'
#' Video thumbnails are always saved as JPEG files regardless of the video
#' format. The thumbnail path is automatically adjusted to have a .jpg
#' extension.
#'
#' Thumbnail directories are created recursively as needed.
#'
#' This function performs file system writes and does not return a value.
#' It is recommended to test on a subset of files before processing
#' large collections.
#'
#' @return Invisibly returns \code{NULL}. The primary effect is the creation
#'   of thumbnail image files on disk.
#'
#' @examples
#' \dontrun{
#' meta <- load_metadata("metadata.csv")
#' unflatten_paths(meta, image_dir = "media")
#' generate_thumbnails(
#'   meta,
#'   image_dir = "media",
#'   thumb_dir = "thumbnails",
#'   thumb_width = 400,
#'   video_seek_time = 2
#' )
#' }
#'
#' @seealso \code{\link{load_metadata}}, \code{\link{unflatten_paths}}
#'
#' @export
generate_thumbnails <- function(meta, image_dir, thumb_dir, thumb_width = 300, 
                                video_seek_time = 1, verbose = TRUE) {
    
    # Check dependencies
    if (!requireNamespace("magick", quietly = TRUE)) {
        stop("Package 'magick' is required for image processing. Please install it with: install.packages('magick')")
    }
    
    # Check for ffmpeg (for video processing)
    has_ffmpeg <- check_ffmpeg()
    if (!has_ffmpeg && verbose) {
        warning("ffmpeg not found. Video thumbnails will be skipped. Install ffmpeg to enable video thumbnail generation.")
    }
    
    if (verbose) message("Preparing thumbnail generation...")
    
    # Setup: Build paths
    meta$full_image_path <- file.path(image_dir, meta$image_path)
    meta$thumb_path_orig <- file.path(thumb_dir, meta$thumb_path)
    meta$file_exists <- file.exists(meta$full_image_path)
    meta$is_video <- sapply(meta$full_image_path, is_video)
    
    # Adjust thumbnail paths for videos (change extension to .jpg)
    meta$thumb_path_final <- ifelse(
        meta$is_video,
        sub("\\.[^.]*$", ".jpg", meta$thumb_path_orig),
        meta$thumb_path_orig
    )
    
    # Setup: Create all needed thumbnail directories
    unique_dirs <- unique(dirname(meta$thumb_path_final))
    lapply(unique_dirs, function(d) dir.create(d, recursive = TRUE, showWarnings = FALSE))
    
    # Count what we're working with
    n_total <- nrow(meta)
    n_missing <- sum(!meta$file_exists)
    n_images <- sum(meta$file_exists & !meta$is_video)
    n_videos <- sum(meta$file_exists & meta$is_video)
    
    if (verbose) {
        message(sprintf("Found %d files (%d missing, %d images, %d videos)", 
                        n_total, n_missing, n_images, n_videos))
        if (n_videos > 0 && !has_ffmpeg) {
            message(sprintf("  WARNING: %d videos will be skipped (ffmpeg not available)", n_videos))
        }
        message("Generating thumbnails...")
    }
    
    # Counters
    n_generated <- 0
    n_skipped <- 0
    n_failed <- 0
    n_videos_processed <- 0
    n_images_processed <- 0
    
    # Process each file
    for (idx in seq_len(n_total)) {
        
        # Skip missing files
        if (!meta$file_exists[idx] || is.na(meta$full_image_path[idx])) {
            n_failed <- n_failed + 1
            next
        }
        
        source_file <- meta$full_image_path[idx]
        thumb_file <- meta$thumb_path_final[idx]
        is_vid <- meta$is_video[idx]
        
        # Skip if thumbnail already exists
        if (file.exists(thumb_file)) {
            n_skipped <- n_skipped + 1
            next
        }
        
        # Generate thumbnail based on file type
        success <- FALSE
        
        if (is_vid) {
            # Process video with ffmpeg
            if (has_ffmpeg) {
                success <- create_video_thumbnail(
                    source_file, 
                    thumb_file, 
                    thumb_width, 
                    video_seek_time
                )
                if (success) n_videos_processed <- n_videos_processed + 1
            } else {
                # Skip videos if ffmpeg not available
                n_failed <- n_failed + 1
                next
            }
        } else {
            # Process image with magick
            success <- tryCatch({
                magick::image_read(source_file) %>%
                    magick::image_scale(paste0(thumb_width)) %>%
                    magick::image_write(thumb_file)
                TRUE
            }, error = function(e) {
                if (verbose) message(sprintf("  Error processing image %s: %s", source_file, e$message))
                FALSE
            })
            if (success) n_images_processed <- n_images_processed + 1
        }
        
        if (success) {
            n_generated <- n_generated + 1
        } else {
            n_failed <- n_failed + 1
        }
        
        # Progress report every 100 files
        if (verbose && idx %% 100 == 0) {
            message(sprintf("  %d/%d (%.1f%%) | New: %d (img: %d, vid: %d) | Exists: %d | Failed: %d", 
                            idx, n_total, (idx/n_total)*100, 
                            n_generated, n_images_processed, n_videos_processed,
                            n_skipped, n_failed))
        }
    }
    
    # Final summary
    if (verbose) {
        message("Thumbnail generation complete!")
        message(sprintf("  Total: %d | Generated: %d (images: %d, videos: %d) | Already existed: %d | Failed: %d", 
                        n_total, n_generated, n_images_processed, n_videos_processed,
                        n_skipped, n_failed))
    }
    
    # Update meta with final thumbnail paths
    meta$thumb_path <- meta$thumb_path_final
    
    invisible(NULL)
}