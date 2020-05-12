" Usage
" <leader>gm in visual and normal modes for opening man page.

" Set default values for global variables
if ! exists("g:view_manpage_chars")
	let g:view_manpage_chars = 'A-Za-z0-9_'
endif

" Operator
function! s:ViewManpageOperator(type)

	" Backup default register
	let saved_unnamed_register = @@

	" Select word to look for
	if a:type ==# 'v' || a:type ==# "\<c-v>" " visual mode
		execute "normal! `<v`>y"
	elseif a:type ==# 'char' " normal mode
		execute "normal! `[v`]y"
	else " Not handled
		return
	endif

	" Print `man` output
	call s:DumpManpage(@@)

	" Restore default register
	let @@ = saved_unnamed_register

endfunction

" Open new window and write output of `man` into it.
function! s:DumpManpage(word)

	" Backup default register
	let saved_unnamed_register = @@

	if &filetype ==# 'man_output'
		" Clean current buffer
		execute "normal! 1GVGd"
	else
		" Open new buffer
		vnew
		setlocal filetype=man_output buftype=nofile noswapfile
	endif

	" Insert man output
	execute "read !man -P cat ".shellescape(a:word)

	" Go to first line and remove it (empty line)
	execute "normal! 1Gdd"

	" Restore default register
	let @@ = saved_unnamed_register

endfunction

" View manpage under cursor
function! s:ViewManpageUnderCursor()

	" Backup values
	let l:saved_unnamed_register = @@

	" Copy text under cursor
	let l:selected = SelectAndCopyAtCursorPos(g:view_manpage_chars)

	" View man page
	if l:selected
		call s:DumpManpage(@@)
	endif

	" Restore unnamed register
	let @@ = l:saved_unnamed_register

endfunction

" View man page command
command! -nargs=1 ViewManpage call <SID>DumpManpage(<args>)

" Operator for normal mode
"nnoremap <leader>gm :set operatorfunc=<SID>ViewManpageOperator<cr>g@

" Operator for opening selected manpage in visual mode
vnoremap <leader>gm :<c-u>call <SID>ViewManpageOperator(visualmode())<cr>

" Operator for opening manpage under cursor un normal node
nnoremap <leader>gm :call <SID>ViewManpageUnderCursor()<cr>
