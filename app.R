# GC Wildlife Viewer --------
#
# Conservation Metrics, Inc 
# Author: Abram B. Fleishman and ChatGPT 5 (with Claude Sonnet
# 4.5 to finalize)
#
# This app exposed a map and filters to explore camera trap images.  The goal is
# a intuitive view that could be deployed as part of Guardian Connector CapRover
# deployments.

# 8 Jan 2026
#
# This current version of the app requires 3 data sources: a CSV exported from
# TimeLapse.exe, a folder of images exported from TimeLapse.exe, and a folder of
# image thumbnails generated from the timelapse image export. the dataloader
# will generate the thumbs if they do not exist
# 

# Install missing packages  ----------------------------------------------
required_packages <- c("shiny", "bslib", "dplyr", "lubridate", "janitor", 
                       "sf", "leaflet", "magick")

missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
}

library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(janitor)
library(sf)
library(leaflet)
library(magick)

# Config  ---------------------------------------------------------------

CONFIG <- list(
    
    # datalake_mount =  Sys.getenv("DATALAKE_MOUNT"),
    # gc_wildlife_mount = Sys.getenv("GC_WILDLIFE_MOUNT"),
    datalake_mount = "D:/CIPDP_camera_trap_exports",
    gc_wildlife_mount = "D:/gc_wild",
    
    images = list(
        image_dir = "TimelapseExport",
        thumb_dir = "thumbs2",
        csv_file  = "ImageData.csv",
        thumb_width = 300
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
            "n_individuals"
        )
    )
)
# *_user are the files that the user uploaded
CONFIG$images$image_dir <- file.path(CONFIG$datalake_mount, CONFIG$images$image_dir)
CONFIG$images$thumb_dir <- file.path(CONFIG$datalake_mount, CONFIG$images$thumb_dir)
CONFIG$images$csv_path_user  <- file.path(CONFIG$datalake_mount, CONFIG$images$csv_file)
CONFIG$images$csv_path  <- file.path(CONFIG$gc_wildlife_mount, CONFIG$images$csv_file)

# on app initialization we make a copy of the csv file and future app launches
# use the copy unless reinitialize=TRUE is set.
# These paths are used for serving files

if(file.exists(CONFIG$images$csv_path_user)&&!file.exists(CONFIG$images$csv_path)){
    file.copy(CONFIG$images$csv_path_user, CONFIG$images$csv_path)
} 


load_metadata <- function(csv_path, verbose = TRUE) {
    
    if (!file.exists(csv_path)) {
        stop("Metadata CSV not found: ", csv_path)
    }
    
    if (verbose) message("Loading metadata from: ", csv_path)
    
    meta <- read.csv(csv_path, stringsAsFactors = FALSE) %>%
        janitor::clean_names() %>%
        mutate(site_name = camera)
    
    # Data spoofer - remove when using real dataset
    if(!"site_name" %in% names(meta)) meta$site_name <- meta$camera
    if(!"latitude" %in% names(meta)) meta$latitude <- 0.9 + rnorm(nrow(meta), mean = 0.01, sd = 0.1)
    if(!"longitude" %in% names(meta)) meta$longitude <- 34.6 + rnorm(nrow(meta), mean = 0.01, sd = 0.1)
    
    meta$image_path_flat <- file.path(
        gsub("\\", ".", paste0(meta$relative_path, ".", meta$file), fixed = TRUE)
    )
    
    meta$image_path<-gsub("\\\\", "/", file.path(meta$relative_path,  meta$file))
    meta$thumb_path<-gsub("\\\\", "/", file.path(meta$relative_path,  meta$file))
    
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



# =============================================================================
# DATA LOADING - Industry standard: Load data before app initialization
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("  Guardian Connector: Wildlife Viewer - Data Initialization\n")
cat("================================================================================\n")
cat("\n")

# Load and prepare data before app starts
META_DATA <- tryCatch({
    load_metadata(
        csv_path = CONFIG$images$csv_path,
        verbose = TRUE
    )
}, error = function(e) {
    cat("\n")
    cat("ERROR: Failed to load data\n")
    cat("Message: ", e$message, "\n")
    cat("\n")
    cat("Please check:\n")
    cat("  1. CONFIG$data_mount path is correct\n")
    cat("  2. ImageData.csv exists in that location\n")
    cat("  3. Image files are accessible\n")
    cat("\n")
    stop(e)
})

# unflatten folder structure if exported from timelapse
unflatten_paths <- function(meta, image_dir, verbose = TRUE) {
    
    if (verbose) message("Unflattening folder structure for image paths...")
    
    
    meta$full_path_flat <- file.path(image_dir, meta$image_path_flat)
    meta$full_path <- file.path(image_dir, meta$image_path)
    lapply(unique( dirname(meta$full_path)),function(x)dir.create(x,recursive = T,showWarnings = F))
    file.rename(meta$full_path_flat, meta$full_path)
    
    
}

# if not all the paths exist
if(!all(file.exists(file.path(CONFIG$images$image_dir,META_DATA$image_path)))){
    unflatten_paths(META_DATA, CONFIG$images$image_dir)
}

# genereate thumb nails
generate_thumbnails <- function(meta, image_dir, thumb_dir, thumb_width = 300, verbose = TRUE) {
    
    # Check dependencies
    if (!requireNamespace("magick", quietly = TRUE)) {
        stop("Package 'magick' is required. Please install it with: install.packages('magick')")
    }
    
    if (verbose) message("Preparing thumbnail generation...")
    
    # Setup: Build paths
    meta$full_image_path <- file.path(image_dir, meta$image_path)
    meta$thumb_path <- file.path(thumb_dir, meta$image_path)
    meta$file_exists <- file.exists(meta$full_image_path)
    
    # Setup: Create all needed thumbnail directories
    unique_dirs <- unique(dirname(meta$thumb_path))
    lapply(unique_dirs, function(d) dir.create(d, recursive = TRUE, showWarnings = FALSE))
    
    # Count what we're working with
    n_total <- nrow(meta)
    n_missing <- sum(!meta$file_exists)
    n_to_process <- sum(meta$file_exists)
    
    if (verbose) {
        message(sprintf("Found %d images (%d missing, %d to process)", 
                        n_total, n_missing, n_to_process))
        message("Generating thumbnails...")
    }
    
    # Counters
    n_generated <- 0
    n_skipped <- 0
    n_failed <- 0
    
    # Process each image
    for (idx in seq_len(n_total)) {
        
        # Skip missing files
        if (!meta$file_exists[idx] || is.na(meta$full_image_path[idx])) {
            meta$thumb_path[idx] <- NA
            n_failed <- n_failed + 1
            next
        }
        
        source_file <- meta$full_image_path[idx]
        thumb_file <- meta$thumb_path[idx]
        
        # Skip if thumbnail already exists
        if (file.exists(thumb_file)) {
            n_skipped <- n_skipped + 1
            next
        }
        
        # Generate thumbnail
        success <- tryCatch({
            magick::image_read(source_file) %>%
                magick::image_scale(paste0(thumb_width)) %>%
                magick::image_write(thumb_file)
            TRUE
        }, error = function(e) {
            FALSE
        })
        
        if (success) {
            n_generated <- n_generated + 1
        } else {
            meta$thumb_path[idx] <- NA
            n_failed <- n_failed + 1
        }
        
        # Progress report every 100 images
        if (verbose && idx %% 100 == 0) {
            message(sprintf("  %d/%d (%.1f%%) | New: %d | Exists: %d | Failed: %d", 
                            idx, n_total, (idx/n_total)*100, 
                            n_generated, n_skipped, n_failed))
        }
    }
    
    # Final summary
    if (verbose) {
        message("Thumbnail generation complete!")
        message(sprintf("  Total: %d | Generated: %d | Already existed: %d | Failed: %d", 
                        n_total, n_generated, n_skipped, n_failed))
    }
    
    # return(meta)
}

# only makes thumb if doesn't exist
generate_thumbnails(meta = meta,
                    image_dir = CONFIG$images$image_dir,
                    thumb_dir = CONFIG$images$thumb_dir,
                    thumb_width = CONFIG$images$thumb_width)

cat("\n")
cat("================================================================================\n")
cat("  Ready to launch app!\n")
cat("================================================================================\n")
cat("\n")

# Filters module  --------------------------------------------------------
filtersUI <- function(id){
    ns <- NS(id)
    tagList(
        selectInput(ns("site_name"), "Site", choices = NULL, multiple = TRUE),
        selectInput(ns("field"), "Field", choices = NULL),
        uiOutput(ns("value_ui")),
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
            updateSelectInput(session, "site_name", choices = sites, selected = sites)
            
            cols <- names(df)
            choices <- intersect(c("common_name","local_name","camera","favorite",
                                   "n_individuals","deployment", "notes"), cols)
            if(length(choices) == 0) choices <- cols
            updateSelectInput(session, "field", choices = choices, selected = choices[1])
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
        
        output$value_ui <- renderUI({
            req(input$field)
            vals <- unique(data()[[input$field]])
            selectizeInput(ns("values"), "Values", choices = sort(na.omit(vals)), multiple = TRUE)
        })
        
        filtered <- reactive({
            df <- data()
            req(df)
            out <- df
            
            if(!is.null(input$site_name) && length(input$site_name) > 0){
                out <- out[out$site_name %in% input$site_name, , drop = FALSE]
            }
            
            if(!is.null(input$field) && !is.null(input$values) && length(input$values) > 0){
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
            updateSelectInput(session, "site_name", selected = sites)
            updateSelectizeInput(session, "values", selected = character(0))
            if("date_time" %in% names(df)){
                dmin <- as.Date(min(df$date_time, na.rm = TRUE))
                dmax <- as.Date(max(df$date_time, na.rm = TRUE))
                updateSliderInput(session, "date_range", value = c(dmin, dmax))
            }
            updateSliderInput(session, "timeofday", value = c(0,23))
        })
        
        set_filter <- function(site_name = NULL, field = NULL, values = NULL, date_range = NULL, timeofday = NULL){
            if(!is.null(site_name)) updateSelectInput(session, "site_name", selected = site_name)
            if(!is.null(field)) updateSelectInput(session, "field", selected = field)
            if(!is.null(values)) updateSelectizeInput(session, "values", selected = values)
            if(!is.null(date_range) && length(date_range) == 2) updateSliderInput(session, "date_range", value = as.Date(date_range))
            if(!is.null(timeofday) && length(timeofday) == 2) updateSliderInput(session, "timeofday", value = timeofday)
            invisible(TRUE)
        }
        
        list(filtered = filtered, set_filter = set_filter)
    })
}

# Map module (Leaflet)  --------------------------------------------------
mapUI <- function(id, height="200px"){
    ns <- NS(id)
    leafletOutput(ns("map"), height = height)
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
            
            leaflet(pts) %>%
                addTiles() %>%
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

# Explorer module (thumbnails + preview)  --------------------------------
explorerUI <- function(id) {
    ns <- NS(id)
    
    # Minimal CSS for grid layout and native aspect ratio
    gallery_css <- sprintf("
        #%s { display: grid; grid-template-columns: repeat(3, 1fr); gap: 12px; padding: 8px; }
        .thumb { width: 100%%; border-radius: 6px; background: #f0f0f0; cursor: pointer; 
                 display: flex; align-items: center; justify-content: center; }
        .thumb img { width: 100%%; height: auto; display: block; }
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
            card_header("Images"),
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
            df |> arrange(desc(date_time))
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
                src <- if (!is.na(row$thumb_path) && nzchar(row$thumb_path)) {
                    file.path("thumbs",row$thumb_path)
                } else {
                    file.path("camimg",row$image_path)
                    
                }
                
                tags$div(
                    class = "thumb",
                    onclick = sprintf(
                        "Shiny.setInputValue('%s', %d, {priority:'event'});",
                        session$ns("thumb_click"), i
                    ),
                    tags$img(src = src, loading = "lazy")
                )
            })
        })
        
        observeEvent(input$thumb_click, {
            df <- items()
            i <- as.integer(input$thumb_click)
            req(i >= 1, i <= nrow(df))
            
            selected_site_rv$site  <- df$site_name[i]
            selected_site_rv$source <- "explorer"
            
            drawer_trigger(list(index = i, data = df[i, , drop = FALSE]))
        }, ignoreInit = TRUE)
    })
}

# Image Drawer Module  ---------------------------------------------------
drawerUI <- function(id) {
    ns <- NS(id)
    
    # Minimal CSS for drawer positioning and animation
    drawer_css <- "
        .drawer-overlay { position: fixed; inset: 0; background: rgba(0,0,0,0.5); 
                          z-index: 9998; display: none; }
        .drawer-overlay.open { display: block; }
        .drawer { position: fixed; top: 0; right: -70vw; width: 70vw; height: 100vh; 
                  background: white; box-shadow: -2px 0 8px rgba(0,0,0,0.15); z-index: 9999; 
                  transition: right 0.3s ease-in-out; overflow-y: auto; }
        .drawer.open { right: 0; }
        .drawer-image { width: 100%; border-radius: 8px; margin-bottom: 16px; }
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
    ", ns("close"), ns("prev"), ns("nexxt"))
    
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
                tags$h5("Image Details", class = "mb-0"),
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
        current_index <- reactiveVal(NULL)
        
        observeEvent(trigger_data(), {
            req(trigger_data())
            is_open(TRUE)
            current_index(trigger_data()$index)
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
            
            idx <- current_index()
            new_idx <- max(1, idx - 1)
            if (new_idx != idx) current_index(new_idx)
        })
        
        observeEvent(input$nexxt, {
            req(is_open())
            df <- all_data()
            req(df, nrow(df) > 0)
            
            idx <- current_index()
            new_idx <- min(nrow(df), idx + 1)
            if (new_idx != idx) current_index(new_idx)
        })
        
        output$content <- renderUI({
            req(current_index())
            df <- all_data()
            req(df, nrow(df) > 0)
            
            idx <- current_index()
            row <- df[idx, , drop = FALSE]
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
            
            tagList(
                div(
                    class = "mb-3",
                    div(class = "text-muted small mb-2",
                        sprintf("Image %d of %d", idx, nrow(df))
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
                
                if (!is.na(row$image_path)) {
                    tags$img(src = file.path("camimg",row$image_path), class = "drawer-image")
                },
                
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

# App UI  ----------------------------------------------------------------
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

# Server  ----------------------------------------------------------------
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

shinyApp(ui, server)