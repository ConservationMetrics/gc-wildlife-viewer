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

## file storage

The app relies on the `ImageData.csv` file.  If this file is modified or removed
the app will crash. To avoid this scenario we set up the app with two volume
mounts, the `datalake` (where users have access and can rename, delete, etc via
FileBrowswer) and a `gcwildlife` where only admin have access.  The app will
check for the file in the gcwildlife mount and if it doesn't exist it will make
a copy of the file there, and then read from that location.

*Not yet supported*
- Full export: Use **File → Export or import data to/from a csv file → Export all data (folder data, image/video data) to csv files...** within Timelapse. 

We will need to put these exported files in ...

TODO: set up with all image data

