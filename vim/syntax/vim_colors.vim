if exists("b:current_syntax")
	finish
endif

runtime! syntax/vim.vim syntax/vim/*.vim

function! s:DefineSyntaxColorFgBg(fgcol, bgcol) abort
	exec 'syntax match vimColorFg'.a:fgcol.'Bg'.a:bgcol.' "ctermfg='.a:fgcol.' \+ctermbg='.a:bgcol.'" containedIn=ALL'
	exec 'highlight link vimColorFg'.a:fgcol.'Bg'.a:bgcol.' ColorFg'.a:fgcol.'Bg'.a:bgcol
endfunction

function! s:DefineSyntaxColorFg(fgcol) abort
	let l:bgcol = 0
	exec 'syntax match vimColorFg'.a:fgcol.' "ctermfg='.a:fgcol.'" containedIn=ALL'
	exec 'highlight link vimColorFg'.a:fgcol.' ColorFg'.a:fgcol.'Bg'.l:bgcol
endfunction

for bgcol in range(254, 0, -1)
	call s:DefineSyntaxColorFgBg(255, bgcol)
endfor
for bgcol in range(255, 1, -1)
	call s:DefineSyntaxColorFgBg(0, bgcol)
endfor
for fgcol in range(254, 0, -1)
	call s:DefineSyntaxColorFgBg(fgcol, 255)
endfor
for fgcol in range(255, 1, -1)
	call s:DefineSyntaxColorFgBg(fgcol, 0)
endfor

for fgcol in range(255, 0, -1)
	call s:DefineSyntaxColorFg(fgcol)
endfor

let b:current_syntax = "vim_colors"
