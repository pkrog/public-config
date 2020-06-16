# vi: ft=sh

# Set background
feh --bg-fill -d --randomize ~/data/wallpapers/

# Read X configuration (for urxvt, ...)
xrdb -merge .Xresources

# Set compose key
xmodmap -e "keysym Alt_R = Multi_key"

# Set DPMS (display Energy Star settings)
xset dpms 240 480 720

case "$XSESSION" in
	i3|i3wm)    XSESSION=i3;;
	xmonad)     XSESSION=xmonad;;
	xfce|xfce4) XSESSION=startxfce4;;
	*)          XSESSION=i3;;
esac
exec $XSESSION
