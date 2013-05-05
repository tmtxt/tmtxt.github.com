---
layout: post
showtn: yes
title: "VLC - Normalize media files"
description: ""
category: Misc
thumbnail: 
tags: [vlc]
---
{% include JB/setup %}

Often, when I have a large number of media files playing in VLC, the most
annoying problem for me is that for each file, I have to manually adjust the
volume for them (so that they are not to loud). Moreover, for those music video
files, I have to deal with the issue about screen resolution and ratio. My
screen's ratio is 16:10 but most of my videos are in 4:3 or 16:9 ratio.

The word normalize here means that to force all the media files to behave in a
same way so that I can enjoy them without worrying to adjust for each file.

# Normalize Audio

![Normalize Audio](/files/2013-04-27-vlc-normalize-media-files/audio.png)

<!-- more -->

Open VLC Preferences panel, head to the **Audio** tab, select **Volume
Normalizer** and adjust the number in the beside textbox (from 0.5-10). The lower
the number is, the lower the sound would be and the higher the volume you need
to adjust. Try open some media files and then figure out which number is best
suitable for you.

# Auto Fit Videos to Screen

![All](/files/2013-04-27-vlc-normalize-media-files/all.png)

Open VLC Preferences again, this time select **All** to show advanced
configuration panel.

![Video](/files/2013-04-27-vlc-normalize-media-files/video.png)

Head to **Video** section, concentrate on 2 textbox **Video Cropping** and
**Source Aspect Ratio**. Depending on your favor that you want VLC to crop or
stretch your video files, input your screen ratio to the appropriate textbox.
