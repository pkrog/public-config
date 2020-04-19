for bgcol in range(255, 0, -1)
	for fgcol in range(255, 0, -1)
		if bgcol != fgcol && (fgcol == 0 || fgcol == 255 || bgcol == 0 || bgcol == 255)
    		exec 'syntax match vimColorFg'.fgcol.'Bg'.bgcol.' "ctermfg='.fgcol.' \+ctermbg='.bgcol.'" containedIn=ALL'
    		exec 'highlight link vimColorFg'.fgcol.'Bg'.bgcol.' ColorFg'.fgcol.'Bg'.bgcol

			" Define single foreground colors
			if bgcol == 0
				exec 'syntax match vimColorFg'.fgcol.' "ctermfg='.fgcol.'" containedIn=ALL'
    			exec 'highlight link vimColorFg'.fgcol.' ColorFg'.fgcol.'Bg'.bgcol
			endif
		endif
	endfor
endfor
