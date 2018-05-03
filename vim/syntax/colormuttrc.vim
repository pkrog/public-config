source $VIMRUNTIME/syntax/muttrc.vim

let fgcol = 255
while fgcol >= 0

	" Define foreground/background couple colors
	if fgcol == 0 || fgcol == 255
		let bgcol = 255
		while bgcol >= 0
			if bgcol != fgcol
    			exec 'syntax match colormuttrcbg'.bgcol.'fg'.fgcol.' "color'.fgcol.' \+color'.bgcol.'" containedIn=ALL'
    			exec 'hi colormuttrcbg'.bgcol.'fg'.fgcol.' ctermbg='.bgcol.' ctermfg='.fgcol
			endif
			let bgcol = bgcol - 1
		endwhile
	endif

	" Define single foreground colors
	if fgcol > 0
		exec 'syntax match colormuttrcfg'.fgcol.' "color'.fgcol.' \+\(default\|color0\)" containedIn=ALL'
    	exec 'hi colormuttrcfg'.fgcol.' ctermfg='.fgcol.' ctermbg=black'
	endif

	let fgcol = fgcol - 1
endwhile

