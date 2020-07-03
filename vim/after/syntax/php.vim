" Redefine links of https://github.com/StanAngeloff/php.vim

highlight link phpDocComment    Comment
highlight link phpCommentTitle  Title
highlight link phpDocTags       Special
highlight link phpDocParam      Type
highlight link phpDocIdentifier Identifier

" Set default values for global variables
if ! exists("g:php_search_chars")
	let g:php_search_chars = 'A-Za-z0-9_'
endif

function! s:DumpPhpManualPage(key)
	let l:link = 'https://www.php.net/manual-lookup.php?pattern='.a:key
	call DumpWebpage(l:link)
	execute "normal! jV/".a:key."\<cr>kd"
endfunction

function! s:PhpSearchManualOperator(type)

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

	" Print PHP manual page
	call s:DumpPhpManualPage(@@)

	" Restore default register
	let @@ = saved_unnamed_register

endfunction

function! s:PhpSearchManualUnderCursor()

	" Backup values
	let l:saved_unnamed_register = @@

	" Copy text under cursor
	let l:selected = SelectAndCopyAtCursorPos(g:php_search_chars)

	" View man page
	if l:selected
		call s:DumpPhpManualPage(@@)
	endif

	" Restore unnamed register
	let @@ = l:saved_unnamed_register

endfunction

" Operator for searching selected word in visual mode
vnoremap <leader>sm :<c-u>call <SID>PhpSearchManualOperator(visualmode())<cr>

" Operator for searching for word under cursor in normal node
nnoremap <leader>sm :call <SID>PhpSearchManualUnderCursor()<cr>
