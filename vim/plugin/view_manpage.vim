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

	let l:selected = 0

	" Backup values
	let saved_unnamed_register = @@
	let saved_cursor = getcurpos()

	let l:cur_char = matchstr(getline('.'), '\%' . col('.') . 'c.')
	if l:cur_char =~? "^\\v[".g:view_manpage_chars."]$"
		" Search backward for first good char
		if col('.') != 1
			let l:search_start = "?\\v(^|[^".g:view_manpage_chars."])\<cr>"
			execute "normal! ".l:search_start
		endif

		" Search forward for first good char
		let l:cur_char = matchstr(getline('.'), '\%' . col('.') . 'c.')
		if l:cur_char !~? "^\\v[".g:view_manpage_chars."]$"
			let l:search_first_good_char = "/\\v[".g:view_manpage_chars."]\<cr>"
			execute "normal! ".l:search_first_good_char
		endif

		" Search for end
		let l:search_end = "/\\v([^".g:view_manpage_chars."]+|$)\<cr>"
		execute "normal! v".l:search_end."hy"

		let l:selected = 1
	endif

	" Restore cursor position
	call setpos('.', saved_cursor)

	" View man page
	if l:selected
		call s:DumpManpage(@@)
	endif

	" Restore unnamed register
	let @@ = saved_unnamed_register

endfunction

" View man page command
command! -nargs=1 ViewManpage call <SID>DumpManpage(<args>)

" Operator for normal mode
"nnoremap <leader>gm :set operatorfunc=<SID>ViewManpageOperator<cr>g@

" Operator for opening selected manpage in visual mode
vnoremap <leader>gm :<c-u>call <SID>ViewManpageOperator(visualmode())<cr>

" Operator for opening manpage under cursor un normal node
nnoremap <leader>gm :call <SID>ViewManpageUnderCursor()<cr>
