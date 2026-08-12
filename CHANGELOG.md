# 2026-08-11 - Allow site_name formapping to be set per deployment PR #27

* adds a configurable argument to the config that lets the user set the column names from which we build the site_name which is used in mapping to determine unique sites/points.

# 2026-08-11 - Added deployment data support

* this release adds support for deployment data. The app now reads a deployment CSV file and uses the lat/lon values to display the deployment on the map.

# 2026-02-16 - Added video support

* this release provides support for video. This was implimented by:
    - track media type for each media file
    - generate thumbnails for video files
        - The way I did this was to write a wrapper function that using [ffmpeg](https://www.ffmpeg.org) to create a jpeg thumbnail for the video file.  there might be a much smarter way to do this.  
    - allow the media viewer drawer to play videos
 
# 2026-01-08 Original image support

* Initial release