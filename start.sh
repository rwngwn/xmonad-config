#!/bin/bash

# need edited xmonad-start to contain:
#
#
## DBE - to start custom scripts
#if [ -x ~/.xmonad/start.sh ] ; then
# ~/.xmonad/start.sh
#fi

stalonetray & 
nm-applet &
bluetooth-applet &
setxkbmap -option caps:swapescape

