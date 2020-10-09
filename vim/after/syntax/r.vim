" Set default values for global variables
if ! exists("g:r_search_chars")
	let g:r_search_chars = 'A-Za-z0-9_\\.'
endif

" Open new window and write output of `R help` into it.
if ! exists("b:inside_r_help_output") || ! b:inside_r_help_output
function! s:DumpRHelpPage(word)

	" Backup default register
	let saved_unnamed_register = @@

	if &filetype ==# 'r_help_output'
		" Clean current buffer
		execute "normal! 1GVGd"
	else
		" Open new buffer
		vnew
		setlocal filetype=r_help_output buftype=nofile noswapfile
	endif

	" Insert man output
	execute "read !Rscript -e '?".shellescape(a:word)."'"

	" Remove backspace control characters (ctrl-H)
	execute "normal! :%s/_\\%x08//g\<cr>"

	" Go to first line and remove it (empty line)
	execute "normal! 1Gdd"

	" Restore default register
	let @@ = saved_unnamed_register

endfunction

function! s:RSearchManualOperator(type)

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

	" View R help page
	call s:DumpRHelpPage(@@)

	" Restore default register
	let @@ = saved_unnamed_register

endfunction

function! s:RSearchManualUnderCursor()

	" Backup values
	let l:saved_unnamed_register = @@

	" Copy text under cursor
	let l:selected = SelectAndCopyAtCursorPos(g:r_search_chars)

	" View R help page
	if l:selected
		call s:DumpRHelpPage(@@)
	endif

	" Restore unnamed register
	let @@ = l:saved_unnamed_register

endfunction
endif

" View R help page command
command! -nargs=1 ViewRHelpPage call <SID>DumpRHelpPage(<args>)

" Operator for searching selected word in visual mode
vnoremap <leader>sl :<c-u>call <SID>RSearchManualOperator(visualmode())<cr>

" Operator for searching for word under cursor in normal node
nnoremap <leader>sl :call <SID>RSearchManualUnderCursor()<cr>
