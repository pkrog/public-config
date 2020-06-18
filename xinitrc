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

case "$XSESSION" in
	i3|i3wm)    XSESSION=i3;;
	xmonad)     XSESSION=xmonad;;
	xfce|xfce4) XSESSION=startxfce4;;
	*)          XSESSION=xmonad;;
esac
exec $XSESSION
