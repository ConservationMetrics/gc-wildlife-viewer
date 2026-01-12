
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
#' @param verbose Logical. If \code{TRUE} (default), progress messages are
#'   printed to the console.
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
#' @importFrom utils read.csv
#' @importFrom janitor clean_names
#' @export

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
    
    # Build flattened path (for TimeLapse exports with backslashes)
    meta$image_path_flat <- file.path(
        gsub("\\", ".", paste0(meta$relative_path, ".", meta$file), fixed = TRUE)
    )
    
    # Build proper hierarchical paths
    meta$image_path <- gsub("\\\\", "/", file.path(meta$relative_path, meta$file))
    meta$thumb_path <- gsub("\\\\", "/", file.path(meta$relative_path, meta$file))
    
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


#' Generate Thumbnail Images from Camera Trap Photographs
#'
#' Creates thumbnail images for camera trap photographs based on metadata
#' produced by \code{load_metadata()}. Thumbnails are written to disk in a
#' directory structure that mirrors the hierarchical image paths.
#'
#' The function reads source images from \code{image_dir}, rescales them
#' to a fixed width while preserving aspect ratio, and writes the results
#' to \code{thumb_dir}. Existing thumbnails are skipped. Missing or unreadable
#' images are recorded as failures.
#'
#' @param meta A data.frame containing image metadata. Must include
#'   \code{image_path} and \code{thumb_path} columns.
#' @param image_dir Character scalar. Base directory containing the
#'   original image files.
#' @param thumb_dir Character scalar. Base directory where thumbnails
#'   will be written.
#' @param thumb_width Numeric. Target width of generated thumbnails in
#'   pixels. Aspect ratio is preserved. Default is \code{300}.
#' @param verbose Logical. If \code{TRUE} (default), progress and summary
#'   messages are printed to the console.
#'
#' @details
#' For each row in \code{meta}, the function attempts to read the image at
#' \code{file.path(image_dir, image_path)} and write a resized version to
#' \code{file.path(thumb_dir, thumb_path)}. Thumbnail directories are created
#' recursively as needed.
#'
#' The function relies on the \pkg{magick} package for image I/O and
#' resizing. An error is thrown if the package is not available.
#'
#' This function performs file system writes and does not return a value.
#' It is recommended to test on a subset of images before processing
#' large collections.
#'
#' @return Invisibly returns \code{NULL}. The primary effect is the creation
#'   of thumbnail image files on disk.
#'
#' @examples
#' \dontrun{
#' meta <- load_metadata("metadata.csv")
#' unflatten_paths(meta, image_dir = "images")
#' generate_thumbnails(
#'   meta,
#'   image_dir = "images",
#'   thumb_dir = "thumbnails",
#'   thumb_width = 400
#' )
#' }
#'
#' @seealso \code{\link{load_metadata}}, \code{\link{unflatten_paths}}
#'
#' @export
generate_thumbnails <- function(meta, image_dir, thumb_dir, thumb_width = 300, verbose = TRUE) {
    
    # Check dependencies
    if (!requireNamespace("magick", quietly = TRUE)) {
        stop("Package 'magick' is required. Please install it with: install.packages('magick')")
    }
    
    if (verbose) message("Preparing thumbnail generation...")
    
    # Setup: Build paths
    meta$full_image_path <- file.path(image_dir, meta$image_path)
    meta$thumb_path <- file.path(thumb_dir, meta$thumb_path)
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