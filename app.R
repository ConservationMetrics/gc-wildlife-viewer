# app.R
# Camera trap explorer — Leaflet version with right drawer
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
    data_mount = "D:/git_repos/survey-dashboard-shiny/data_mount",
    
    images = list(
        image_dir = "images",
        thumb_dir = "images/thumbs",
        csv_file  = "images/ImageData.csv",
        thumb_width = 300 #px
    ),
    
    map = list(
        default_zoom = 6
    ),
    
    explorer = list(
        batch_size = 20,
        thumb_height = 140,
        primary_meta = c(
            "camera",
            "date_time",
            "local_name",
            "common_name",
            "n_individuals"
        )
    )
    
)

CONFIG$images$image_dir <- file.path(CONFIG$data_mount, CONFIG$images$image_dir)
CONFIG$images$thumb_dir <- file.path(CONFIG$data_mount, CONFIG$images$thumb_dir)
CONFIG$images$csv_path  <- file.path(CONFIG$data_mount, CONFIG$images$csv_file)

# Helper: load metadata and generate image thumbnails  ------------------
load_metadata <- function(
        path,
        image_dir,
        thumb_dir,
        thumb_width = 300   # width in pixels for thumbnails
){
    
    if (!requireNamespace("magick", quietly = TRUE)) {
        stop("Package 'magick' is required for thumbnail generation. Please install it.")
    }
    
    if (!file.exists(path)) {
        stop("Metadata CSV not found: ", path)
    }
    
    if (!dir.exists(thumb_dir)) dir.create(thumb_dir, recursive = TRUE)
    
    if (file.exists(path)) {
        meta <- read.csv(path, stringsAsFactors = FALSE) %>%
            janitor::clean_names() %>%
            mutate(site_name = camera)
        
        # data spoofer
        # this section is only needed until we havea trial dataset to use
        if(!"site_name" %in% names(meta)) meta$site_name <- meta$camera
        if(!"latitude" %in% names(meta)) meta$latitude <- 0.9 + rnorm(nrow(meta), mean = 0.01, sd = 0.1)
        if(!"longitude" %in% names(meta)) meta$longitude <- 34.6 + rnorm(nrow(meta), mean = 0.01, sd = 0.1)
        # build paths that work locally from a timelapse export
        if(!"image_path" %in% names(meta)) {
            meta$image_path <- file.path(
                "camimg",
                "TimelapseExport",
                gsub("\\",".", paste0(meta$relative_path,".", meta$file), fixed = TRUE)
            )
        }
        
        # Date-time parsing
        if("date_time" %in% names(meta)){
            meta$date_time <- as.POSIXct(meta$date_time, tz = "UTC",
                                         tryFormats = c("%Y-%m-%dT%H:%M:%OS",
                                                        "%Y-%m-%d %H:%M:%OS",
                                                        "%Y-%m-%d"))
        } else meta$date_time <- NA
        
        # ---- Generate thumbnails ----
        meta$thumb_path <- sapply(meta$image_path, function(img_path) {
            full_path <- file.path(image_dir, gsub("camimg/","",img_path))
            if (!file.exists(full_path)) return(NA)
            
            # Thumbnail filename
            thumb_file <- file.path(thumb_dir, paste0(basename(img_path)))
            
            # Generate thumbnail if it doesn't exist
            if (!file.exists(thumb_file)) {
                try({
                    magick::image_read(full_path) %>%
                        magick::image_scale(paste0(thumb_width)) %>%  # width x auto height
                        magick::image_write(thumb_file)
                }, silent = TRUE)
            }
            # Return relative path for Shiny
            file.path("thumbs", basename(img_path))
        })
        
        return(meta)
    }
}

# Filters module  --------------------------------------------------------
filtersUI <- function(id){
    ns <- NS(id)
    tagList(
        # site selector, defauls to all sites
        selectInput(ns("site_name"), "Site", choices = NULL, multiple = TRUE),
        # dropdown with entries for each metadata field
        selectInput(ns("field"), "Observation metadata field", choices = NULL),
        # this populates with all the unique values from the field above
        uiOutput(ns("value_ui")),
        dateRangeInput(ns("date_range"), "Date range"),
        sliderInput(ns("timeofday"), "Time of day (hour)", min = 0, max = 23, value = c(0,23)),
        actionButton(ns("clear"), "Clear filters", class = "btn-sm btn-secondary")
    )
}

filtersServer <- function(id, data){
    moduleServer(id, function(input, output, session){
        ns <- session$ns
        
        # Initialize filter choices
        observeEvent(data(), {
            df <- data()
            req(df)
            
            # Site_name filter
            sites <- sort(unique(df$site_name))
            updateSelectInput(session, "site_name", choices = sites, selected = sites)
            
            # Dynamic field/value filter
            cols <- names(df)
            choices <- intersect(c("common_name","local_name","camera","favorite",
                                   "n_individuals","deployment", "notes"), cols)
            if(length(choices) == 0) choices <- cols
            updateSelectInput(session, "field", choices = choices, selected = choices[1])
            
            if("date_time" %in% cols){
                dmin <- min(df$date_time, na.rm = TRUE)
                dmax <- max(df$date_time, na.rm = TRUE)
                updateDateRangeInput(session, "date_range", start = as.Date(dmin), end = as.Date(dmax))
            }
        }, ignoreNULL = TRUE)
        
        output$value_ui <- renderUI({
            req(input$field)
            vals <- unique(data()[[input$field]])
            selectizeInput(ns("values"), "Select values", choices = sort(na.omit(vals)), multiple = TRUE)
        })
        
        filtered <- reactive({
            df <- data()
            req(df)
            out <- df
            
            # Apply site_name filter
            if(!is.null(input$site_name) && length(input$site_name) > 0){
                out <- out[out$site_name %in% input$site_name, , drop = FALSE]
            }
            
            # Apply dynamic field/value filter
            if(!is.null(input$field) && !is.null(input$values) && length(input$values) > 0){
                out <- out[out[[input$field]] %in% input$values, , drop = FALSE]
            }
            
            # Apply date range filter
            if(!is.null(input$date_range) && !any(is.na(input$date_range)) && "date_time" %in% names(out)){
                rstart <- as.POSIXct(input$date_range[1], tz = "UTC")
                rend <- as.POSIXct(input$date_range[2]) + lubridate::days(1) - 1
                out <- out[is.na(out$date_time) | (out$date_time >= rstart & out$date_time <= rend), , drop = FALSE]
            }
            
            # Apply time-of-day filter
            if(!is.null(input$timeofday) && "date_time" %in% names(out)){
                hrs <- lubridate::hour(out$date_time)
                out <- out[is.na(out$date_time) | (hrs >= input$timeofday[1] & hrs <= input$timeofday[2]), , drop = FALSE]
            }
            
            out
        })
        
        # Clear button
        observeEvent(input$clear, {
            df <- data()
            sites <- sort(unique(df$site_name))
            updateSelectInput(session, "site_name", selected = sites)
            updateSelectizeInput(session, "values", selected = character(0))
            if("date_time" %in% names(df)){
                dmin <- min(df$date_time, na.rm = TRUE)
                dmax <- max(df$date_time, na.rm = TRUE)
                updateDateRangeInput(session, "date_range", start = as.Date(dmin), end = as.Date(dmax))
            }
            updateSliderInput(session, "timeofday", value = c(0,23))
        })
        
        set_filter <- function(site_name = NULL, field = NULL, values = NULL, date_range = NULL, timeofday = NULL){
            if(!is.null(site_name)) updateSelectInput(session, "site_name", selected = site_name)
            if(!is.null(field)) updateSelectInput(session, "field", selected = field)
            if(!is.null(values)) updateSelectizeInput(session, "values", selected = values)
            if(!is.null(date_range) && length(date_range) == 2) updateDateRangeInput(session, "date_range", start = as.Date(date_range[1]), end = as.Date(date_range[2]))
            if(!is.null(timeofday) && length(timeofday) == 2) updateSliderInput(session, "timeofday", value = timeofday)
            invisible(TRUE)
        }
        
        list(filtered = filtered, set_filter = set_filter)
    })
}


# Map module (Leaflet)  --------------------------------------------------
mapUI <- function(id, height="600px"){
    ns <- NS(id)
    leafletOutput(ns("map"), height = height)
}

mapServer <- function(id, all_sites_df, filtered_sites, selected_site_rv) {
    
    moduleServer(id, function(input, output, session) {
        
        ns <- session$ns
        
        # ---- initial render: ALL sites ----
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
        
        observeEvent(input$map_marker_click, {
            selected_site_rv$site   <- as.character(input$map_marker_click$id)
            selected_site_rv$source <- "map"
        })
        
        # ---- reactive styling based on filters + selection ----
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
            
            # ---- selection halo (visual only) ----
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
        
        # ---- map click sets selection ONLY ----
        observeEvent(input$map_marker_click, {
            selected_site_rv$site <- as.character(input$map_marker_click$id)
        })
    })
}




# Explorer module (thumbnails + preview)  --------------------------------
explorerUI <- function(id, height = "80vh") {
    ns <- NS(id)
    
    tagList(
        tags$style(
            HTML(sprintf("
        #%s {
          height: %s;
          overflow-y: auto;
        }

        #%s {
          display: grid;
          grid-template-columns: repeat(3, 1fr);
          gap: 12px;
          padding: 8px;
        }

        .thumb {
          width: 100%%;
          overflow: hidden;
          border-radius: 6px;
          background: #f0f0f0;
          cursor: pointer;
          display: flex;
          align-items: center;
          justify-content: center;
        }

        .thumb img {
          width: 100%%;
          height: auto;
          display: block;
        }
      ",
                         ns("scroll"),
                         height,
                         ns("gallery")
            ))
        ),
        
        tags$script(
            HTML(sprintf("
        $(document).ready(function() {
          const el = document.getElementById('%s');
          if (!el) {
            console.error('Scroll element not found');
            return;
          }

          el.addEventListener('scroll', function() {
            if (el.scrollTop + el.clientHeight >= el.scrollHeight - 150) {
              Shiny.setInputValue('%s', Math.random(), {priority: 'event'});
            }
          });
        });
      ",
                         ns("scroll"),
                         ns("load_more")
            ))
        ),
        
        bslib::card(
            full_screen = TRUE,
            card_header("Images"),
            div(
                id = ns("scroll"),
                div(
                    id = ns("gallery"),
                    uiOutput(ns("thumbs"))
                )
            )
        )
    )
}

explorerServer <- function(id, data, selected_site_rv, drawer_trigger,
                           batch_size = 20) {
    
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
                    row$thumb_path
                } else {
                    row$image_path
                }
                
                tags$div(
                    class = "thumb",
                    onclick = sprintf(
                        "Shiny.setInputValue('%s', %d, {priority:'event'});",
                        session$ns("thumb_click"), i
                    ),
                    tags$img(
                        src = src,
                        loading = "lazy"
                    )
                )
            })
        })
        
        observeEvent(input$thumb_click, {
            df <- items()
            i <- as.integer(input$thumb_click)
            req(i >= 1, i <= nrow(df))
            
            # ---- IMPORTANT: selection only ----
            selected_site_rv$site  <- df$site_name[i]
            selected_site_rv$source <- "explorer"
            
            # Trigger drawer with image data
            drawer_trigger(list(
                index = i,
                data = df[i, , drop = FALSE]
            ))
            
        }, ignoreInit = TRUE)
    })
}


# Image Drawer Module  ---------------------------------------------------
drawerUI <- function(id) {
    ns <- NS(id)
    
    # Minimal CSS for drawer behavior
    drawer_css <- "
        .drawer-overlay { position: fixed; inset: 0; background: rgba(0,0,0,0.5); z-index: 9998; display: none; }
        .drawer-overlay.open { display: block; }
        .drawer { position: fixed; top: 0; right: -70vw; width: 70vw; height: 100vh; 
                  background: white; box-shadow: -2px 0 8px rgba(0,0,0,0.15); z-index: 9999; 
                  transition: right 0.3s ease-in-out; overflow-y: auto; }
        .drawer.open { right: 0; }
        .drawer-image { width: 100%; border-radius: 8px; margin-bottom: 16px; }
        .details-content { display: none; }
        .details-content.open { display: block; }
    "
    
    tagList(
        tags$style(HTML(drawer_css)),
        
        # Keyboard navigation
        tags$script(HTML(sprintf("
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
        ", ns("close"), ns("prev"), ns("nexxt")))),
        
        # Overlay
        tags$div(
            class = "drawer-overlay",
            id = ns("overlay"),
            onclick = sprintf("Shiny.setInputValue('%s', true, {priority: 'event'});", ns("close"))
        ),
        
        # Drawer
        tags$div(
            class = "drawer",
            id = ns("drawer"),
            # Header
            div(
                class = "d-flex justify-content-between align-items-center p-3 border-bottom bg-light",
                tags$h5("Image Details", class = "mb-0"),
                actionButton(ns("close_btn"), "×", 
                             class = "btn-close", 
                             onclick = sprintf("Shiny.setInputValue('%s', true, {priority: 'event'});", ns("close")))
            ),
            # Content
            div(class = "p-3", uiOutput(ns("content")))
        )
    )
}

drawerServer <- function(id, trigger_data, all_data, primary_fields = CONFIG$explorer$primary_meta) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        is_open <- reactiveVal(FALSE)
        details_open <- reactiveVal(FALSE)
        current_index <- reactiveVal(NULL)
        
        # Open drawer when triggered
        observeEvent(trigger_data(), {
            req(trigger_data())
            is_open(TRUE)
            details_open(FALSE)
            current_index(trigger_data()$index)
            session$sendCustomMessage("toggleDrawer", list(open = TRUE))
        })
        
        # Close drawer
        observeEvent(c(input$close, input$close_btn), {
            is_open(FALSE)
            session$sendCustomMessage("toggleDrawer", list(open = FALSE))
        })
        
        # Navigate to previous image
        observeEvent(input$prev, {
            req(is_open())
            df <- all_data()
            req(df, nrow(df) > 0)
            
            idx <- current_index()
            new_idx <- max(1, idx - 1)
            
            if (new_idx != idx) {
                current_index(new_idx)
            }
        })
        
        # Navigate to nexxt image
        observeEvent(input$nexxt, {
            req(is_open())
            df <- all_data()
            req(df, nrow(df) > 0)
            
            idx <- current_index()
            new_idx <- min(nrow(df), idx + 1)
            
            if (new_idx != idx) {
                current_index(new_idx)
            }
        })
        
        # Toggle details section
        observeEvent(input$toggle_details, {
            details_open(!details_open())
        })
        
        output$content <- renderUI({
            req(current_index())
            df <- all_data()
            req(df, nrow(df) > 0)
            
            idx <- current_index()
            row <- df[idx, , drop = FALSE]
            path <- row$image_path
            meta <- as.list(row)
            
            # Separate primary and additional metadata
            available_primary <- intersect(primary_fields, names(meta))
            additional_fields <- setdiff(names(meta), available_primary)
            
            # Helper function to format field names
            format_label <- function(field) {
                gsub("_", " ", tools::toTitleCase(field))
            }
            
            # Helper function to create metadata row
            meta_row <- function(field, value) {
                div(
                    class = "d-flex justify-content-between py-2 border-bottom",
                    tags$strong(format_label(field)),
                    span(as.character(value))
                )
            }
            
            tagList(
                # Primary metadata at top
                div(
                    class = "mb-3",
                    # Image counter
                    div(
                        class = "text-muted small mb-2",
                        sprintf("Image %d of %d", idx, nrow(df))
                    ),
                    # Primary fields - horizontal layout
                    lapply(available_primary, function(field) {
                        tags$span(
                            class = "me-3",
                            tags$span(
                                class = "text-muted small",
                                paste0(format_label(field), ": ")
                            ),
                            tags$span(
                                class = "fw-bold",
                                as.character(meta[[field]])
                            )
                        )
                    })
                ),
                
                # Image
                if (!is.na(path)) {
                    tags$img(src = path, class = "drawer-image")
                },
                
                # Additional details accordion
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
ui <- fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    
    # Custom message handler for drawer toggle
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
    
    titlePanel("Guardian connector: Wildlife Viewer"),
    
    drawerUI("image_drawer"),
    
    sidebarLayout(
        sidebarPanel(
            width = 3,
            # Map at top of sidebar
            bslib::card(
                full_screen = FALSE,
                card_header("Map"),
                mapUI("map_main", height="400px")
            ),
            # Filters below map
            hr(),
            h4("Filters"),
            filtersUI("filters")
        ),
        mainPanel(
            width = 9,
            explorerUI("explorer")
        )
    )
)

server <- function(input, output, session) {
    
    meta <- load_metadata(
        path = CONFIG$images$csv_path,
        image_dir = CONFIG$images$image_dir,
        thumb_dir = CONFIG$images$thumb_dir,
        thumb_width = CONFIG$images$thumb_width
    )
    
    r_meta <- reactiveVal(meta)
    
    filters_res <- filtersServer("filters", data = reactive(r_meta()))
    
    selected_site  <- reactiveValues(site = NULL, source = NULL)
    
    # Drawer trigger
    drawer_data <- reactiveVal(NULL)
    
    # ---- derived availability state ----
    filtered_sites <- reactive({
        df <- filters_res$filtered()
        req(df)
        unique(df$site_name)
    })
    
    # ---- map (always all sites) ----
    mapServer(
        "map_main",
        all_sites_df  = reactive(r_meta()),
        filtered_sites = filtered_sites,
        selected_site_rv = selected_site
    )
    
    # ---- explorer (filtered images only) ----
    explorerServer(
        "explorer",
        data = reactive(filters_res$filtered()),
        selected_site_rv = selected_site,
        drawer_trigger = drawer_data
    )
    
    # ---- drawer ----
    drawerServer(
        "image_drawer",
        trigger_data = drawer_data,
        all_data = reactive(filters_res$filtered()),
        primary_fields = CONFIG$explorer$primary_meta
    )
    
    # ---- ONLY map clicks update filters ----
    observeEvent(selected_site$site, {
        
        req(selected_site$site)
        
        if (!identical(selected_site$source, "map")) {
            return()
        }
        
        filters_res$set_filter(site_name = selected_site$site)
        
        df <- filters_res$filtered()
        sel <- df %>% filter(site_name == selected_site$site)
        
        if (nrow(sel) > 0) {
            drawer_data(list(
                index = 1,
                data = sel[1, , drop = FALSE]
            ))
        }
        
    })
    
}


shinyApp(ui, server)