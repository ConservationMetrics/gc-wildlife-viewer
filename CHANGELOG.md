# 2026-08-11 - Support dynamic fields in filter PR #22

* Change hard coded field names in the field drop down filter to dynamically generated list based on all columns that have >1 value and do not have another filter already.

# 2026-08-11 - Added deployment data support  PR #16

* this release adds support for deployment data. The app now reads a deployment CSV file and uses the lat/lon values to display the deployment on the map.

# 2026-02-16 - Added video support PR #13

* this release provides support for video. This was implimented by:
    - track media type for each media file
    - generate thumbnails for video files
        - The way I did this was to write a wrapper function that using [ffmpeg](https://www.ffmpeg.org) to create a jpeg thumbnail for the video file.  there might be a much smarter way to do this.  
    - allow the media viewer drawer to play videos
 
# 2026-01-08 Original image support

* Initial release