Related: ../README.txt

Read prep, then install, then publish.


==Bind the "Windows Key" to the "Start Menu" in Linux
If using LXDE.org, add the following to ~/.config/openbox/lxde-rc.xml

<keybind key="Super_L">
 <action name="Execute">
  <command>lxpanelctl menu</command>
 </action>
</keybind>

