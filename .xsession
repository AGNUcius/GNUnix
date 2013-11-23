setxkbmap -option ctrl:nocaps
xsetroot -solid gray25

# Set resolution for VGA port
# this is a no-op if VGA port is not in use
xrandr --output VGA1 --mode 1024x768

# You can set the path for your entire X session by
# adjusting the "DisplayManager*userPath" variable
# found at /etc/X11/wdm/wdm-config (if using wdm)
