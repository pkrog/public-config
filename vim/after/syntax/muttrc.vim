" Define coloring of color definitions
let bgcol = 0
for fgcol in range(255, 1, -1)
	exec 'syntax match muttrcColorFg'.fgcol.' "color'.fgcol.' " containedIn=muttrcColor contained'
	exec 'highlight link muttrcColorFg'.fgcol.' ColorFg'.fgcol.'Bg'.bgcol
endfor
