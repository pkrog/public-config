# vi: ft=sh

case "$XSESSION" in
	i3|i3wm) XSESSION=i3;;
	*) XSESSION=startxfce4;;
esac
exec $XSESSION
