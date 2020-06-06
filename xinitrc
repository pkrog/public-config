# vi: ft=sh

case "$XSESSION" in
	i3|i3wm) XSESSION=i3;;
	xmonad)  XSESSION=xmonad;;
	*)       XSESSION=startxfce4;;
esac
exec $XSESSION
