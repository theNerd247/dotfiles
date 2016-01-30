############################## 
# /$HOME/.xinitrc
# 
# called when startx or xinit is called
# 
# This calls an X Server client program to run
############################## 
#
#set variables from old /etc/skel/xinitrc
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

winmgr=$2

#default wmgr
if [[ $winmgr == "" ]] ; then 
	winmgr="xmonad"
fi

setupXstd()
{
	#save the DBUS_SESSION_BUS_ADDRESS variable so notify-send will work with
	#crontab
	echo $DBUS_SESSION_BUS_ADDRESS > $HOME/.dbusaddr

	#set the cursor type
	xsetroot -cursor_name left_ptr
	xset m 4 4 #set the sensitivity of the mouse

	setupTouchpad	

	#personal daemons
	$HOME/src/scripts/chkscreens.sh 

	#setup Xresources
	xrdb -merge $HOME/.Xresources
	export SAL_USE_VCLPLUGIN=gen

	#feh --bg-fill /home/noah/.background
	
	#start the screensaver daemon 
	xscreensaver &

	#start my terminal daemon
	exec urxvtd --quiet --fork -o & 
}

#touchpad options
setupTouchpad()
{
	synclient HorizTwoFingerScroll=1

	syndaemon -t -k -d &
}


#EXPORT VARIABLES
export XDG_MENU_PREFIX=lxde-
export XDG_CURRENT_DESKTOP=LXDE
export GTK2_RC_FILES="$HOME/.gtkrc-2.0" 
export XAUTHORITY="/home/noah/.Xauthority"

setupXstd

case $winmgr in 
	xmonad)
		exec xmonad ;;
	ob)
		exec openbox-session ;;
	lxde)
		exec startlxde ;;
	*)
		exec $winmgr ;;
esac
