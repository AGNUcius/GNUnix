if not exist "%~pn1" MD "%~pn1"
dism /Mount-Wim /WimFile:%~f1 /index:1 /MountDir:%~pn1  /ReadOnly
