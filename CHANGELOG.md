# 2026-02-16 - Added video support

* this release provides support for video. This was implimented by:
    - track media type for each media file
    - generate thumbnails for video files
        - The way I did this was to write a wrapper function that using [ffmpeg](https://www.ffmpeg.org) to create a jpeg thumbnail for the video file.  there might be a much smarter way to do this.  
    - allow the media viewer drawer to play videos
 
# 2026-01-08 Original image support

* Initial release