set MountDir=%~pn1

if not '%2'=='' set MountDir=%2
if not exist "%MountDir%" MD "%MountDir%"
dism /Mount-Wim /WimFile:%~f1 /index:1 "/MountDir:%MountDir%"

