# Guardian Connector Wildlife Viewer --------
#
# Conservation Metrics, Inc 
# Author: Abram B. Fleishman and ChatGPT 5 (with Claude Sonnet 4.5 to finalize)
#
# This app exposes a map and filters to explore camera trap images and videos. 
# The goal is an intuitive view that could be deployed as part of Guardian 
# Connector CapRover deployments.
#
# This current version of the app requires: a CSV exported from TimeLapse.exe, 
# a folder of images (and or videos) exported from TimeLapse.exe, a folder 
# of image and video thumbnails.
#
# When running locally, data is read from ../data_mount/
# When running in Docker, set APP_DATA_PATH to the container mount point.

# EXTERNAL DATA PATH -----------------------------------------------------
APP_DATA_PATH <- Sys.getenv("APP_DATA_PATH", unset = "../data_mount")

# LOAD PACKAGES ------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(janitor)
library(leaflet)
library(magick)

# LOAD CUSTOM FUNCTIONS ----------------------------------------------------
source("utils.r")

# CONFIG -------------------------------------------------------------------
CONFIG <- list(
    datalake_mount = file.path(APP_DATA_PATH, "datalake/camera_traps"),
    gc_wildlife_mount = file.path(APP_DATA_PATH, "gc-wildlife"),
    
    # For Abram's local development
    # datalake_mount = "D:/CIPDP_camera_trap_exports",
    # gc_wildlife_mount = "D:/gc_wild",
    
    images = list(
        image_dir = "TimelapseExport",
        thumb_dir = "thumbs2",
        csv_file  = "ImageData.csv",
        thumb_width = 300
    ),
    
    deployment_data=list(
        # TODO: these might make sense to be ENV vars
        deployment_data_path = file.path(APP_DATA_PATH, "datalake/camera_traps/deployment.csv"),
        # these are the column names into which we will split the
        # "relative_path" column from the timelapse export ImageData.csv.  They
        # are then used to join with the deployment.csv
        rel_path_parts= c("deployment", "region", "camera","location_name"),
        site_name_cols = c("location_name", "camera_name", "region")
    ),
    
    map = list(
        default_zoom = 6
    ),
    
    explorer = list(
        batch_size = 20,
        primary_meta = c(
            "camera",
            "date_time",
            "local_name",
            "common_name",
            "n_individuals",
            "media_type"  
        )
    )
)

# Set up file paths
CONFIG$images$image_dir <- file.path(CONFIG$datalake_mount, CONFIG$images$image_dir)
CONFIG$images$thumb_dir <- file.path(CONFIG$datalake_mount, CONFIG$images$thumb_dir)
CONFIG$images$csv_path_user  <- file.path(CONFIG$datalake_mount, CONFIG$images$csv_file)

# safe path for tabular data
CONFIG$images$csv_path  <- file.path(CONFIG$gc_wildlife_mount, CONFIG$images$csv_file)

# On app initialization we make a copy of the csv file and future app launches
# only recopy if there is no copy at the destination
if(file.exists(CONFIG$images$csv_path_user) && !file.exists(CONFIG$images$csv_path)){
    dir.create(dirname(CONFIG$images$csv_path), recursive = TRUE, showWarnings = FALSE)
    file.copy(CONFIG$images$csv_path_user, CONFIG$images$csv_path)
}

cat("\n")
cat("================================================================================\n")
cat("  Guardian Connector: Wildlife Viewer - Data Initialization\n")
cat("================================================================================\n")
cat("\n")

# LOAD AND PREPARE DATA BEFORE APP STARTS -----------------------------------
META_DATA <- tryCatch({
    
    # Step 1: Load metadata
    cat("STEP 1: Loading metadata...\n")
    meta <- load_metadata(
        csv_path = CONFIG$images$csv_path,
        deployment_data_path = CONFIG$deployment_data$deployment_data_path,
        rel_path_parts = CONFIG$deployment_data$rel_path_parts,
        site_name_cols = CONFIG$deployment_data$site_name_cols,
        verbose = TRUE
    )
    
    # NEW: Add media type detection
    cat("\nSTEP 1b: Detecting media types...\n")
    meta$media_type <- sapply(meta$file, get_media_type)
    message("  Found ", sum(meta$media_type == "image", na.rm = TRUE), " images")
    message("  Found ", sum(meta$media_type == "video", na.rm = TRUE), " videos")
    
    # Step 2: Unflatten folder structure if needed
    cat("\nSTEP 2: Checking folder structure...\n")
    if (!all(file.exists(file.path(CONFIG$images$image_dir, meta$image_path)))) {
        unflatten_paths(
            meta = meta,
            image_dir = CONFIG$images$image_dir,
            verbose = TRUE
        )
    } else {
        message("Folder structure already correct, skipping unflatten step.")
    }
    
    # Step 3: Generate thumbnails for images
    cat("\nSTEP 3: Generating thumbnails for images...\n")
    generate_thumbnails(
        meta = meta,
        image_dir = CONFIG$images$image_dir,
        thumb_dir = CONFIG$images$thumb_dir,
        thumb_width = CONFIG$images$thumb_width,
        verbose = TRUE
    )
    
    #  Step 4: Set up video thumbnail paths
    cat("\nSTEP 4: Setting up video thumbnail paths...\n")
    
    cat("\n")
    cat("================================================================================\n")
    cat("  Data initialization complete!\n")
    cat("================================================================================\n")
    cat("\n")
    
    meta
    
}, error = function(e) {
    cat("\n")
    cat("================================================================================\n")
    cat("  ERROR: Failed to load data\n")
    cat("================================================================================\n")
    cat("Message: ", e$message, "\n")
    cat("\n")
    cat("Please check:\n")
    cat("  1. CONFIG paths are correct\n")
    cat("  2. ImageData.csv exists at: ", CONFIG$images$csv_path, "\n")
    cat("  3. Image files are accessible at: ", CONFIG$images$image_dir, "\n")
    cat("  4. Thumbnail directory is writable: ", CONFIG$images$thumb_dir, "\n")
    cat("\n")
    stop(e)
})

# FILTERS MODULE -----------------------------------------------------------
filtersUI <- function(id){
    ns <- NS(id)
    tagList(
        selectInput(ns("site_name"), "Site", choices = NULL, multiple = TRUE),
        
        selectInput(ns("media_type"), "Media Type", 
                    choices = c("All" = "all", "Images" = "image", "Videos" = "video"),
                    selected = "all"),
        
        selectInput(ns("field"), "Field", choices = NULL),
        selectizeInput(
            ns("values"),
            "Values",
            choices = NULL,
            selected = character(0),
            multiple = TRUE
        ),
        uiOutput(ns("date_range_ui")),
        sliderInput(ns("timeofday"), "Time (hr)", min = 0, max = 23, value = c(0,23)),
        actionButton(ns("clear"), "Clear", class = "btn-sm btn-secondary w-100")
    )
}

filtersServer <- function(id, data){
    moduleServer(id, function(input, output, session){
        ns <- session$ns
        
        observeEvent(data(), {
            df <- data()
            req(df)
            
            sites <- sort(unique(df$site_name))
            updateSelectInput(session, "site_name", choices = sites, selected = character(0))
            
            # names from df that are not NA
            cols <- names(df)[ !sapply(df,function(x)length(unique(x))<=1)]
            
            choices <- cols[!(cols%in%c("datetime",
                                        "deployment_datetime",
                                        "retrieval_datetime",
                                        "deployment_date",
                                        "deployment_time",
                                        "retrieval_date", 
                                        "retrieval_time",
                                        "image_path_flat",
                                        "image_path",
                                        "thumb_path"))]
            
            if (length(choices) == 0) choices <- cols
            
            choices <- c("None" = "", choices)
            
            updateSelectInput(
                session,
                "field",
                choices = choices,
                selected = ""
            )
        }, ignoreNULL = TRUE)
        
        output$date_range_ui <- renderUI({
            df <- data()
            req(df)
            
            if("date_time" %in% names(df)){
                dmin <- as.Date(min(df$date_time, na.rm = TRUE))
                dmax <- as.Date(max(df$date_time, na.rm = TRUE))
                
                sliderInput(
                    ns("date_range"),
                    "Date",
                    min = dmin,
                    max = dmax,
                    value = c(dmin, dmax),
                    timeFormat = "%d-%b\n%Y", 
                    width = "90%"
                )
            }
        })
        
        observeEvent(input$field, {
            req(input$field != "")
            
            vals <- unique(data()[[input$field]])
            
            updateSelectizeInput(
                session,
                "values",
                choices = sort(na.omit(vals)),
                selected = character(0)
            )
        }, ignoreInit = TRUE)
        
        filtered <- reactive({
            df <- data()
            req(df)
            out <- df
            
            if (
                !is.null(input$site_name) &&
                length(input$site_name) > 0 &&
                length(input$site_name) < length(unique(df$site_name))
            ) {
                out <- out[out$site_name %in% input$site_name, , drop = FALSE]
            }
            
            if(!is.null(input$media_type) && input$media_type != "all"){
                out <- out[out$media_type == input$media_type, , drop = FALSE]
            }
            
            if (
                !is.null(input$field) &&
                nzchar(input$field) &&
                !is.null(input$values) &&
                length(input$values) > 0
            ) {
                out <- out[out[[input$field]] %in% input$values, , drop = FALSE]
            }
            
            if(!is.null(input$date_range) && !any(is.na(input$date_range)) && "date_time" %in% names(out)){
                rstart <- as.POSIXct(input$date_range[1], tz = "UTC")
                rend <- as.POSIXct(input$date_range[2]) + lubridate::days(1) - 1
                out <- out[is.na(out$date_time) | (out$date_time >= rstart & out$date_time <= rend), , drop = FALSE]
            }
            
            if(!is.null(input$timeofday) && "date_time" %in% names(out)){
                hrs <- lubridate::hour(out$date_time)
                out <- out[is.na(out$date_time) | (hrs >= input$timeofday[1] & hrs <= input$timeofday[2]), , drop = FALSE]
            }
            
            out
        })
        
        observeEvent(input$clear, {
            df <- data()
            sites <- sort(unique(df$site_name))
            updateSelectInput(session, "site_name", selected = character(0))
            updateSelectInput(session, "media_type", selected = "all")  
            updateSelectizeInput(session, "field", choices = character(0), selected = character(0) )
            updateSelectInput(session, "values", selected = character(0))
            if("date_time" %in% names(df)){
                dmin <- as.Date(min(df$date_time, na.rm = TRUE))
                dmax <- as.Date(max(df$date_time, na.rm = TRUE))
                updateSliderInput(session, "date_range", value = c(dmin, dmax))
            }
            updateSliderInput(session, "timeofday", value = c(0,23))
        })
        
        set_filter <- function(site_name = NULL, field = NULL, values = NULL, date_range = NULL, timeofday = NULL, media_type = NULL){
            if(!is.null(site_name)) updateSelectInput(session, "site_name", selected = site_name)
            if(!is.null(media_type)) updateSelectInput(session, "media_type", selected = media_type) 
            if(!is.null(field)) updateSelectInput(session, "field", selected = field)
            if(!is.null(values)) updateSelectizeInput(session, "values", selected = values)
            if(!is.null(date_range) && length(date_range) == 2) updateSliderInput(session, "date_range", value = as.Date(date_range))
            if(!is.null(timeofday) && length(timeofday) == 2) updateSliderInput(session, "timeofday", value = timeofday)
            invisible(TRUE)
        }
        
        list(filtered = filtered, set_filter = set_filter)
    })
}

# MAP MODULE (Leaflet)  ----------------------------------------------------
mapUI <- function(id, height="200px"){
    ns <- NS(id)
    wrap_id <- ns("map_wrap")
    map_id  <- ns("map")
    tagList(
        div(
            id = wrap_id,
            class = "map-resize-wrap",
            style = sprintf("height:%s;", height),
            leafletOutput(map_id, height = "100%"),
            div(class = "map-resize-handle", title = "Drag to resize map")
        ),
        # REVIEW: Is there a Shiny-native way to set up resize bars, to avoid
        # needing to inject JavaScript?
        # c.f. https://github.com/ConservationMetrics/gc-wildlife-viewer/pull/32#pullrequestreview-4929861068
        tags$script(HTML(sprintf("
            (function() {
                const wrapId = '%s', mapId = '%s', key = 'gc-wildlife-map-height';
                const minH = 140;
                const maxH = () => Math.round(window.innerHeight * 0.7);
                const invalidate = () => {
                    try {
                        const m = HTMLWidgets.find('#' + mapId);
                        if (m && m.invalidateSize) m.invalidateSize();
                    } catch (e) {}
                };
                const wrap = document.getElementById(wrapId);
                if (!wrap || wrap.dataset.resizeInit) return;
                wrap.dataset.resizeInit = '1';
                const saved = localStorage.getItem(key);
                if (saved) wrap.style.height = saved;
                wrap.querySelector('.map-resize-handle').addEventListener('pointerdown', (e) => {
                    e.preventDefault();
                    const startY = e.clientY, startH = wrap.offsetHeight;
                    const move = (ev) => {
                        wrap.style.height = Math.min(maxH(), Math.max(minH, startH + ev.clientY - startY)) + 'px';
                    };
                    const up = () => {
                        document.removeEventListener('pointermove', move);
                        document.removeEventListener('pointerup', up);
                        localStorage.setItem(key, wrap.style.height);
                    };
                    document.addEventListener('pointermove', move);
                    document.addEventListener('pointerup', up);
                });
                new ResizeObserver(invalidate).observe(wrap);
            })();
        ", wrap_id, map_id)))
    )
}

mapServer <- function(id, all_sites_df, filtered_sites, selected_site_rv) {
    
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        
        output$map <- renderLeaflet({
            df <- all_sites_df()
            req(df)
            
            pts <- df %>%
                filter(!is.na(longitude), !is.na(latitude)) %>%
                group_by(site_name) %>%
                summarize(
                    longitude = mean(longitude),
                    latitude  = mean(latitude),
                    .groups = "drop"
                )
            
            leaflet(pts, options = leafletOptions(attributionControl = FALSE)) %>%
                # TODO: Allow users to set their basemap option, or do 
                # https://github.com/ConservationMetrics/gc-wildlife-viewer/issues/6
                addProviderTiles(providers$Esri.WorldImagery) %>%
                addProviderTiles(providers$CartoDB.DarkMatterOnlyLabels) %>%
                addCircleMarkers(
                    lng = ~longitude,
                    lat = ~latitude,
                    layerId = ~site_name,
                    radius = 7,
                    color = "#ff5500",
                    fillOpacity = 0.8
                )
        })
        
        observe({
            df_all <- all_sites_df()
            req(df_all)
            
            active_sites <- filtered_sites()
            selected     <- selected_site_rv$site
            
            pts <- df_all %>%
                filter(!is.na(longitude), !is.na(latitude)) %>%
                group_by(site_name) %>%
                summarize(
                    longitude = mean(longitude),
                    latitude  = mean(latitude),
                    .groups = "drop"
                ) %>%
                mutate(
                    active = site_name %in% active_sites,
                    color  = ifelse(active, "#ff5500", "#bdbdbd"),
                    radius = ifelse(active, 7, 5),
                    alpha  = ifelse(active, 0.8, 0.3)
                )
            
            proxy <- leafletProxy(ns("map"), data = pts) %>%
                clearMarkers() %>%
                addCircleMarkers(
                    lng = ~longitude,
                    lat = ~latitude,
                    layerId = ~site_name,
                    radius = ~radius,
                    color = ~color,
                    fillOpacity = ~alpha,
                    label = ~site_name
                )
            
            if (!is.null(selected) && selected %in% pts$site_name) {
                sel <- pts %>% filter(site_name == selected)
                proxy %>%
                    addCircleMarkers(
                        lng = sel$longitude,
                        lat = sel$latitude,
                        radius = 12,
                        color = "#00AAFF",
                        fillOpacity = 0.25,
                        layerId = paste0(sel$site_name, "_halo")
                    )
            }
        })
        
        observeEvent(input$map_marker_click, {
            selected_site_rv$site <- as.character(input$map_marker_click$id)
            selected_site_rv$source <- "map"
        })
    })
}

# EXPLORER MODULE (thumbnails + preview)  ----------------------------------
explorerUI <- function(id) {
    ns <- NS(id)
    
    gallery_css <- sprintf("
        #%s { display: grid; grid-template-columns: repeat(3, 1fr); gap: 12px; padding: 8px; }
        .thumb { width: 100%%; border-radius: 6px; background: #f0f0f0; cursor: pointer; 
                 display: flex; align-items: center; justify-content: center; position: relative; }
        .thumb img { width: 100%%; height: auto; display: block; }
        .badge-video { position: absolute; top: 8px; right: 8px; background-color: #ff4444; 
                      color: white; padding: 4px 10px; border-radius: 4px; font-size: 11px; 
                      font-weight: bold; z-index: 10; text-shadow: 0 1px 2px rgba(0,0,0,0.3); }
    ", ns("gallery"))
    
    tagList(
        tags$style(HTML(gallery_css)),
        tags$script(HTML(sprintf("
            $(document).ready(function() {
                const el = document.getElementById('%s');
                if (el) {
                    el.addEventListener('scroll', function() {
                        if (el.scrollTop + el.clientHeight >= el.scrollHeight - 150) {
                            Shiny.setInputValue('%s', Math.random(), {priority: 'event'});
                        }
                    });
                }
            });
        ", ns("scroll"), ns("load_more")))),
        
        bslib::card(
            full_screen = TRUE,
            max_height = "calc(100vh - 100px)",
            card_header("Images & Videos"),  
            bslib::card_body(
                id = ns("scroll"),
                style = "overflow-y: auto;",
                div(id = ns("gallery"),
                    uiOutput(ns("thumbs"))
                )
            )
        )
    )
}

explorerServer <- function(id, data, selected_site_rv, drawer_trigger, batch_size = 20) {
    
    moduleServer(id, function(input, output, session) {
        
        items <- reactive({
            df <- data()
            req(df)
            df %>% arrange(desc(date_time))
        })
        
        addResourcePath("camimg", CONFIG$images$image_dir)
        addResourcePath("thumbs", CONFIG$images$thumb_dir)
        
        n_loaded <- reactiveVal(batch_size)
        
        observeEvent(input$load_more, {
            df <- items()
            n_loaded(min(nrow(df), n_loaded() + batch_size))
        }, ignoreInit = TRUE)
        
        output$thumbs <- renderUI({
            df <- items()
            req(df)
            
            df <- df[seq_len(min(n_loaded(), nrow(df))), , drop = FALSE]
            
            lapply(seq_len(nrow(df)), function(i) {
                row <- df[i, , drop = FALSE]
                
                if (row$media_type == "video") {
                    # Use video thumbnail
                    src <- if (!is.na(row$thumb_path) && nzchar(row$thumb_path)) {
                        file.path("thumbs", row$thumb_path)
                    } else {
                        # Fallback to a placeholder or first frame
                        "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='300' height='200'%3E%3Crect width='300' height='200' fill='%23333'/%3E%3Ctext x='50%25' y='50%25' fill='white' text-anchor='middle'%3EVIDEO%3C/text%3E%3C/svg%3E"
                    }
                    badge <- tags$span(class = "badge-video", "VIDEO")
                } else {
                    # Use image thumbnail
                    src <- if (!is.na(row$thumb_path) && nzchar(row$thumb_path)) {
                        file.path("thumbs", row$thumb_path)
                    } else {
                        file.path("camimg", row$image_path)
                    }
                    badge <- NULL
                }
                
                tags$div(
                    class = "thumb",
                    onclick = sprintf(
                        "Shiny.setInputValue('%s', %s, {priority:'event'});",
                        session$ns("thumb_click"),
                        jsonlite::toJSON(list(
                            image_path = row$image_path,
                            thumb_path = row$thumb_path,
                            media_type = row$media_type,  
                            thumb_path = row$thumb_path  
                        ), auto_unbox = TRUE)
                    ),
                    tags$img(src = src, loading = "lazy"),
                    badge  
                )
            })
        })
        
        observeEvent(input$thumb_click, {
            df <- items()
            payload <- input$thumb_click
            req(payload$image_path)
            
            row <- df[df$image_path == payload$image_path, , drop = FALSE]
            req(nrow(row) == 1)
            
            selected_site_rv$site   <- row$site_name
            selected_site_rv$source <- "explorer"
            
            drawer_trigger(list(
                image_path = payload$image_path,
                thumb_path = payload$thumb_path,
                media_type = payload$media_type,  
                thumb_path = payload$thumb_path  
            ))
        }, ignoreInit = TRUE)
    })
}

# IMAGE DRAWER MODULE ------------------------------------------------------
drawerUI <- function(id) {
    ns <- NS(id)
    
    drawer_css <- "
        .drawer-overlay { position: fixed; inset: 0; background: rgba(0,0,0,0.5); 
                          z-index: 9998; display: none; }
        .drawer-overlay.open { display: block; }
        .drawer { position: fixed; top: 0; right: -70vw; width: 70vw; height: 100vh; 
                  background: white; box-shadow: -2px 0 8px rgba(0,0,0,0.15); z-index: 9999; 
                  transition: right 0.3s ease-in-out; overflow-y: auto; }
        .drawer.open { right: 0; }
        .drawer-image { width: 100%; border-radius: 8px; margin-bottom: 16px; }
        .drawer-video { width: 100%; border-radius: 8px; margin-bottom: 16px; max-height: 70vh; }
    "
    
    # Keyboard navigation script
    keyboard_js <- sprintf("
        $(document).on('keydown', function(e) {
            var drawer = document.querySelector('.drawer');
            if (!drawer || !drawer.classList.contains('open')) return;
            
            if (e.key === 'Escape') {
                Shiny.setInputValue('%s', true, {priority: 'event'});
            } else if (e.key === 'ArrowLeft') {
                e.preventDefault();
                Shiny.setInputValue('%s', Math.random(), {priority: 'event'});
            } else if (e.key === 'ArrowRight') {
                e.preventDefault();
                Shiny.setInputValue('%s', Math.random(), {priority: 'event'});
            }
        });
    ", ns("close"), ns("prev"), ns("next_img"))
    
    tagList(
        tags$style(HTML(drawer_css)),
        tags$script(HTML(keyboard_js)),
        
        tags$div(
            class = "drawer-overlay",
            id = ns("overlay"),
            onclick = sprintf("Shiny.setInputValue('%s', true, {priority: 'event'});", ns("close"))
        ),
        
        tags$div(
            class = "drawer",
            id = ns("drawer"),
            div(
                class = "d-flex justify-content-between align-items-center p-3 border-bottom bg-light",
                tags$h5("Media Details", class = "mb-0"),  
                actionButton(ns("close_btn"), "×", 
                             class = "btn-close", 
                             onclick = sprintf("Shiny.setInputValue('%s', true, {priority: 'event'});", ns("close")))
            ),
            div(class = "p-3", uiOutput(ns("content")))
        )
    )
}

drawerServer <- function(id, trigger_data, all_data, primary_fields = CONFIG$explorer$primary_meta) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        is_open <- reactiveVal(FALSE)
        current_image_path <- reactiveVal(NULL)
        current_thumb_path <- reactiveVal(NULL)
        current_media_type <- reactiveVal(NULL)  
        
        observeEvent(trigger_data(), {
            req(trigger_data()$image_path)
            
            is_open(TRUE)
            current_image_path(trigger_data()$image_path)
            current_thumb_path(trigger_data()$thumb_path)
            current_media_type(trigger_data()$media_type) 
            
            session$sendCustomMessage("toggleDrawer", list(open = TRUE))
        })
        
        observeEvent(c(input$close, input$close_btn), {
            is_open(FALSE)
            session$sendCustomMessage("toggleDrawer", list(open = FALSE))
        })
        
        observeEvent(input$prev, {
            req(is_open())
            df <- all_data()
            req(df, nrow(df) > 0)
            
            idx <- match(current_image_path(), df$image_path)
            req(!is.na(idx))
            
            new_idx <- max(1, idx - 1)
            current_image_path(df$image_path[new_idx])
            current_thumb_path(df$thumb_path[new_idx])
            current_media_type(df$media_type[new_idx])  
        })
        
        observeEvent(input$next_img, {
            req(is_open())
            df <- all_data()
            req(df, nrow(df) > 0)
            
            idx <- match(current_image_path(), df$image_path)
            req(!is.na(idx))
            
            new_idx <- min(nrow(df), idx + 1)
            current_image_path(df$image_path[new_idx])
            current_thumb_path(df$thumb_path[new_idx])
            current_media_type(df$media_type[new_idx])  
        })
        
        output$content <- renderUI({
            req(current_image_path())
            df <- all_data()
            
            row <- df[df$image_path == current_image_path(), , drop = FALSE]
            req(nrow(row) == 1)
            
            idx <- match(current_image_path(), df$image_path)
            req(!is.na(idx))
            
            meta <- as.list(row)
            
            available_primary <- intersect(primary_fields, names(meta))
            additional_fields <- setdiff(names(meta), available_primary)
            
            format_label <- function(field) {
                gsub("_", " ", tools::toTitleCase(field))
            }
            
            meta_row <- function(field, value) {
                div(
                    class = "d-flex justify-content-between py-2 border-bottom",
                    tags$strong(format_label(field)),
                    span(as.character(value))
                )
            }
            
            # Create appropriate media display (image or video)
            media_display <- if (row$media_type == "video") {
                # VIDEO PLAYER
                video_src <- file.path("camimg", row$image_path)  # Note: assuming video files are in image_path column
                tags$video(
                    src = video_src,
                    controls = "controls",
                    class = "drawer-video",
                    preload = "metadata",
                    tags$p("Your browser does not support the video tag.")
                )
            } else {
                # IMAGE DISPLAY
                if (!is.na(row$image_path)) {
                    tags$img(src = file.path("camimg", row$image_path), class = "drawer-image")
                } else {
                    NULL
                }
            }
            
            tagList(
                div(
                    class = "mb-3",
                    div(class = "text-muted small mb-2",
                        sprintf("Media %d of %d", idx, nrow(df)) 
                    ),
                    lapply(available_primary, function(field) {
                        tags$span(
                            class = "me-3",
                            tags$span(class = "text-muted small",
                                      paste0(format_label(field), ": ")),
                            tags$span(class = "fw-bold",
                                      as.character(meta[[field]]))
                        )
                    })
                ),
                
                media_display,  
                
                bslib::accordion(
                    id = ns("details_accordion"),
                    bslib::accordion_panel(
                        "Additional Details",
                        lapply(additional_fields, function(field) {
                            meta_row(field, meta[[field]])
                        })
                    )
                )
            )
        })
    })
}

# UI -----------------------------------------------------------------------
ui <- page_fillable(
    theme = bs_theme(bootswatch = "flatly"),
    
    tags$script(HTML("
        Shiny.addCustomMessageHandler('toggleDrawer', function(message) {
            const drawer = document.querySelector('.drawer');
            const overlay = document.querySelector('.drawer-overlay');
            
            if (message.open) {
                drawer.classList.add('open');
                overlay.classList.add('open');
            } else {
                drawer.classList.remove('open');
                overlay.classList.remove('open');
            }
        });
    ")),
    
    tags$style(HTML("
        .bslib-sidebar-layout { height: calc(100vh - 60px); }
        .sidebar { overflow-y: auto !important; }
        .map-resize-wrap {
            display: flex;
            flex-direction: column;
            min-height: 140px;
            max-height: 70vh;
        }
        .map-resize-wrap > .html-widget {
            flex: 1 1 auto;
            min-height: 0;
            height: 100% !important;
        }
        .map-resize-handle {
            flex: 0 0 12px;
            cursor: ns-resize;
            display: flex;
            align-items: center;
            justify-content: center;
            background: #ecf0f1;
        }
        .map-resize-handle::before {
            content: '';
            width: 36px;
            height: 4px;
            border-radius: 2px;
            background: #95a5a6;
        }
    ")),
    
    h3("Guardian Connector: Wildlife Viewer", class = "mb-3"),
    
    drawerUI("image_drawer"),
    
    layout_sidebar(
        sidebar = sidebar(
            padding = 3,
            width = 370,
            bslib::card(
                class = "mb-2",
                mapUI("map_main", height = "200px")
            ),
            filtersUI("filters")
        ),
        explorerUI("explorer")
    )
)

# SERVER -------------------------------------------------------------------
server <- function(input, output, session) {
    
    # Use pre-loaded data (industry standard approach)
    r_meta <- reactiveVal(META_DATA)
    
    filters_res <- filtersServer("filters", data = reactive(r_meta()))
    
    selected_site <- reactiveValues(site = NULL, source = NULL)
    drawer_data <- reactiveVal(NULL)
    
    filtered_sites <- reactive({
        df <- filters_res$filtered()
        req(df)
        unique(df$site_name)
    })
    
    mapServer(
        "map_main",
        all_sites_df  = reactive(r_meta()),
        filtered_sites = filtered_sites,
        selected_site_rv = selected_site
    )
    
    explorerServer(
        "explorer",
        data = reactive(filters_res$filtered()),
        selected_site_rv = selected_site,
        drawer_trigger = drawer_data
    )
    
    drawerServer(
        "image_drawer",
        trigger_data = drawer_data,
        all_data = reactive(filters_res$filtered()),
        primary_fields = CONFIG$explorer$primary_meta
    )
    
    observeEvent(selected_site$site, {
        req(selected_site$site)
        
        if (!identical(selected_site$source, "map")) {
            return()
        }
        
        filters_res$set_filter(site_name = selected_site$site)
    })
}

# RUN APP ------------------------------------------------------------------
shinyApp(ui, server)