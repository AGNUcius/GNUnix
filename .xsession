setxkbmap -option ctrl:nocaps
xsetroot -solid gray25
#xscreensaver -no-splash&

# this is harmless if the VGA port is not in use
# xrandr --output VGA1 --mode 1024x768
# xrandr --output VGA1 --mode 1920x1080 --output LVDS1 --mode 1920x1080

# You can set the path for your entire X session by
# adjusting the "DisplayManager*userPath" variable
# found at /etc/X11/wdm/wdm-config (if using wdm)

# why doesn't this work?
#export PATH=${PATH}:${HOME}/bin
