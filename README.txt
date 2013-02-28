GNUnix is not GNU/Linux

GNUnix is an Operating System construction kit
that starts as a very lightweight distribution
then morphs into whatever you want during use.

For now it is just a 'mini' install of UBUNTU
Precise 12.04 with IceWM and Chromium.

The core idea (only partly implemented) is
"Install And Launch When Attempted" (IALWA)
which makes finding and using and fixing the
programs you want much more easy and obvious.

See bin/pkg.IALWA.target for instructions

The high-level view of IALWA is a Star-Menu
filled with all the applications available for
installation, but not yet actually installed.

There is no separate package-manager because
all management is done in one unified place.

Each entry appears greyed-out until installed.

Right-clicking or pausing over a menu entry
reveals a menu containing screenshots, user
reviews, a way to request features and bug
fixes, edit configuration or uninstall.

Once the user tries to use that program, the
package and all dependencies are installed,
and the program is then launched.  The menu
entry is updated to show it is installed.

The window-manager is also be enhanced with
another button (maybe a '+') on each window
as another way to access the features given
when right-clicking the menu-entry.

A global configuration file contains settings
such as "prompt before install", "uninstall
app after X days of disuse", "sort entries by
popularity", etc.


See .GNUnix/prep to begin
