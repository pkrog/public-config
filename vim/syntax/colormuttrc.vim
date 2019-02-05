source $VIMRUNTIME/syntax/muttrc.vim

let fgcol = 255
while fgcol >= 0

	" Define single foreground colors
	if fgcol > 0
		exec 'syntax match colormuttrcfg'.fgcol.' "color'.fgcol.' " containedIn=ALL'
    	exec 'hi colormuttrcfg'.fgcol.' ctermfg='.fgcol.' ctermbg=black'
	endif

	let fgcol = fgcol - 1
endwhile

