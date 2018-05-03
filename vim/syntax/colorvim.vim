source $VIMRUNTIME/syntax/vim.vim

let fgcol = 255
while fgcol >= 0

	" Define foreground/background couple colors
	let bgcol = 255
	while bgcol >= 0
		if bgcol != fgcol && (fgcol == 0 || fgcol == 255 || bgcol == 0 || bgcol == 255)
    		exec 'syntax match colorvimbg'.bgcol.'fg'.fgcol.' "ctermfg='.fgcol.' \+ctermbg='.bgcol.'" containedIn=ALL'
    		exec 'hi colorvimbg'.bgcol.'fg'.fgcol.' ctermbg='.bgcol.' ctermfg='.fgcol
		endif
		let bgcol = bgcol - 1
	endwhile

	" Define single foreground colors
	if fgcol > 0
		exec 'syntax match colorvimfg'.fgcol.' "ctermfg='.fgcol.'" containedIn=ALL'
    	exec 'hi colorvimfg'.fgcol.' ctermfg='.fgcol.' ctermbg=black'
	endif

	let fgcol = fgcol - 1
endwhile
