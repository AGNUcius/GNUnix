GNUnix is not Linux

GNUnix is an Operating System construction kit
that starts as a very lightweight distribution
then morphs into whatever you want during use.

For now it is just a 'mini' install of UBUNTU
Precise 12.04 with IceWM and Chromium.


The core idea (that is not yet implemented) is
"Install And Launch When Attempted" (IALWA)
which makes finding and using and fixing the
programs you want much more easy and obvious.

The high-level view of IALWA is a Start-Menu
filled with all the applications you could ever
possibly install, not yet actually installed.

There would be no separate package-manager
because the Start-Menu seamlessly manages
packages and launches those same programs.

Each entry might be slightly greyed-out if the
app is not yet installed.

I envision the icons, reviews, screenshots,
and other heavy data could be retrieved over
the network (and cached) when the user
pauses over that menu entry.

Once the user tries to use that program, the
package-manager would be invoked to install
that package and any dependencies, and then
the program would be launched, and the menu
entry would be updated to show it is installed.

Right-clicking the menu-entry would allow the
user to uninstall or configure or complain about
bugs or suggest new features, or go to an
online forum, etc.

The window-manager should also be enhanced
to add another button (maybe a '+') that would
be another way to access the features given
when right-clicking the menu-entry.

A global configuration file would contain settings
such as "prompt before install", "uninstall app
after X days of disuse", "sort menu entries
by popularity", etc.


See .GNUnix/prep to begin
