@echo off
if '%3'=='' (
   echo Param1: .wim
   echo Param2: mount dir
   echo Param3: driver dir
   goto :eof
)

if not exist "%2" MD "%2"
dism /Mount-Wim /WimFile:%~f1 /index:1 "/MountDir:%2"
dism "/Image:%2" /Add-Driver "/driver:%3" /recurse
dism /Unmount-Wim "/mountdir:%2" /commit

