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

" View man page command
command! -nargs=1 ViewManpage call <SID>DumpManpage(<args>)

" Operator for normal mode
nnoremap <leader>mp :set operatorfunc=<SID>ViewManpageOperator<cr>g@

" Operator for visual mode
vnoremap <leader>mp :<c-u>call <SID>ViewManpageOperator(visualmode())<cr>
