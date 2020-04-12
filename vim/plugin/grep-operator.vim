" Purpose:
"   Search recursively for selection or word under score inside current
"   folder.
" Usage:
" viw<leader>g Visually select a word, then grep for it.
" <leader>g4w Grep for the next four words.
" <leader>gt; Grep until semicolon.
" <leader>gi[ Grep inside square brackets.

" Grep function
function! s:GrepOperator(type)
	" Backup default register
	let saved_unnamed_register = @@

	" Select word to look for
	if a:type ==# 'v'
		execute "normal! `<v`>y"
	elseif a:type ==# 'char'
		execute "normal! `[v`]y"
	else
		return
	endif

	" Grep recursively for selected text inside current folder
	silent execute "grep! -R " . shellescape(@@) . " ."
	redraw!

	" Open Quickfix window
	copen

	" Restore default register
	let @@ = saved_unnamed_register
endfunction

" Create operator for normal mode
nnoremap <leader>g :set operatorfunc=<SID>GrepOperator<cr>g@

" Create operator for visual mode
vnoremap <leader>g :<c-u>call <SID>GrepOperator(visualmode())<cr>
