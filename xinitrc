# vi: ft=sh

# Read X configuration (for urxvt, ...)
xrdb -merge .Xresources

# Set trayer (for icons)
#trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x191970 --height 12 &

# Set background
feh --bg-fill -d --randomize ~/data/wallpapers/

# Set compose key
xmodmap -e "keysym Alt_R = Multi_key"

# Bind keys
xbindkeys

# Set DPMS (display Energy Star settings)
xset dpms 240 480 720

# Run auto suspend & hibernate
#xautolock -time 5 -locker "systemctl suspend-then-hibernate" &
# TODO Issue: goes to suspend mode event if watching movie or program running.

# Run screen saver
xscreensaver -no-splash &

# Auto hide mouse cursor/pointer
unclutter &

case "$XSESSION" in
	i3|i3wm)    XSESSION=i3;;
	xfce|xfce4) XSESSION=startxfce4;;
	gnome)      XSESSION=gnome-session ; export XDG_SESSION_TYPE=x11 ; export GDK_BACKEND=x11 ;;
	xmonad|*)   XSESSION=xmonad;;
esac
exec $XSESSION
