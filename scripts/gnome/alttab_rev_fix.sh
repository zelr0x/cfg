#!/usr/bin/env sh

# Setting input source switch to Alt-Shift breaks Shift+Alt+Tab
# (Alt-Tab in reverse order) in Gnome Shell.
# Unfortunately, I did not find a solution that allows these
# options to coexist, so this script just sets input source switch
# to Super+Space and resets Alt+Shift allowing Shift+Alt+Tab to work.

gsettings reset org.gnome.desktop.input-sources xkb-options
gsettings set org.gnome.desktop.wm.keybindings switch-input-source "['<Super>Space']"
gsettings set org.gnome.desktop.wm.keybindings switch-input-source-backward "['<Super>Space']"
