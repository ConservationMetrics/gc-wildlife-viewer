# gc-wildlife-viewer: R Shiny app for exploring wildlife media and data 

This app exposed a map and filters to explore camera trap images.  The goal is
a intuitive view that could be deployed as part of Guardian Connector CapRover
deployments.

8 Jan 2026

# Data Setup

This current version of the app requires 3 data sources: a CSV exported from
Timelapse.exe, a folder of images exported from Timelapse.exe, and a folder of
image thumbnails generated from the Timelapse image export.

## Timelapse data export

The Timelapse data are stored in a sqlite DB, but this script is set up to use an exported csv from Timelapse.  When setting up this app you need to export a **selection**. Providing access to the full dataset is not yet supported.  

    - Use the selection menu to select the subset of images that you want to work with (e.g. images marked "Favorite" or images that have non-blank data fields for local_name)
    - Export the tabular data as a CSV file Use **File → Export or import data to/from a csv file → Export image/video data in the current selection to a csv...** within Timelapse.
    - To export the selection of images for the dashboard use **File → Copy Image/video files to another folder → Copy all Image or Video files in the current selection to...** 

The app expects the following file organization: a root folder `{images}` with
the `TimelapseExport` subfolder that houses the images inside.   `ImageData.csv`
(export) should be placed in the `images` folder.  The script is currently
set up to check for and create a `thumbs` subfolder.

## File Storage

The app relies on the `ImageData.csv` file.  If this file is modified or removed
the app will crash. To avoid this scenario we set up the app with two volume
mounts, the `datalake` (where users have access and can rename, delete, etc via
FileBrowser) and a `gc-wildlife` where only admin have access.  The app will
check for the file in the gcwildlife mount and if it doesn't exist it will make
a copy of the file there, and then read from that location.

*Not yet supported*
- Full export: Use **File → Export or import data to/from a csv file → Export all data (folder data, image/video data) to csv files...** within Timelapse. 

We will need to put these exported files in ...

TODO: set up with all image data

## Quick Start

### 1. Add runtime libraries to the Dockerfile

See [Adding R Packages](#adding-r-packages) below.

### 2. Build the Docker Image

The `shiny` user inside the container needs to read files you mount from your host machine. To make this work, build the image with your user's UID/GID:

```bash
docker build --build-arg SHINY_UID=$(id -u) --build-arg SHINY_GID=$(id -g) -t guardiancr.azurecr.io/gc-wildlife-viewer:latest .
```

If deploying to a server where the data files are owned by a different user, use that user's UID/GID instead.
If you installed CapRover on a fresh VM, the correct UID and GID to use there are most likely `1000`.

### 3. Run Locally with Docker

```bash
docker run -p 3838:3838 -v "$(pwd)/data_mount:/data_mount" guardiancr.azurecr.io/gc-wildlife-viewer:latest
```

Then open http://localhost:3838

**Without Docker (for development):** Open `app/shiny-app.Rproj` in RStudio and click "Run App". The app will read from `data_mount/` in the repo root.

### 4. Deploy to CapRover

1. Push the Docker image to a container registry that's hooked up in your CapRover deployment.

2. Create a new app in CapRover, making sure to check **Has Persistent Data**.

3. In **App Configs > Environment Variables**, add:
   ```
   APP_DATA_PATH=/data_mount
   ```

3. In **App Configs > Persistent Directories**, add a volume mount:
   - Path in App: `/data_mount`
   - Map to a host path or named volume containing your data

4. If you need password protection, in **HTTP Settings > Edit HTTP Basic Auth** assign a username and password. This approach does not allow multiple different usernames.

5. Set the **Container HTTP Port** to `3838`.

6. Under the **Deployment** tab, use "Method 6: Deploy via ImageName"


## Project Structure

```
.
├── app/
│   ├── app.R              # Your Shiny application
│   └── shiny-app.Rproj    # RStudio project file
├── data_mount/            # Put data files here for LOCAL development
├── Dockerfile
├── shiny-server.conf
└── README.md
```

## Working with Data

Your app reads data from `APP_DATA_PATH`:
- **Local development:** Defaults to `../data_mount` (relative to `app/`)
- **Docker:** Set to `/data_mount` via the `.Renviron` file in the image

In your R code:

```r
APP_DATA_PATH <- Sys.getenv("APP_DATA_PATH", unset = "../data_mount")

# Helper to build paths
data_path <- function(...) file.path(APP_DATA_PATH, ...)

# Use it
my_data <- read.csv(data_path("my_data.csv"))
```

## Adding R Packages

Install packages in the Dockerfile, not at runtime. Edit the Dockerfile:

```dockerfile
RUN R -e "install.packages(c('dplyr', 'ggplot2', 'plotly'), repos='https://cloud.r-project.org')"
```

This keeps container startup fast and ensures reproducible builds.

### Troubleshooting

If you encounter errors like:

```
[INFO] shiny-server - Error getting worker: Error: The application exited during initialization.
```

That is an indicator that you might be missing an R package. See https://github.com/rstudio/shiny-server/issues/353 for more information on how to debug this by turning on and checking logs.

## Get building! Your next steps are:

1. Edit `app/app.R` to build your application
2. Add data files to `data_mount/` for local testing
3. Update the Dockerfile to install any packages you need
4. Rename `shiny-app.Rproj` if you like
