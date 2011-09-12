#!/bin/bash

xrdb -merge .Xresources
xrdb ~/.Xdefaults &
gnome-power-manager&
krb5-auth-dialog&
sealert&
puplet&
nm-applet --sm-disable&
gnome-screensaver &
export PATH="$PATH:~/.cabal/bin"
exec xmonad
