function! s:ManOperator(type)

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

	" Open new buffer
	vnew
	setlocal filetype=man_output buftype=nofile noswapfile

	" Insert man output
	execute "read !man -P cat ".shellescape(@@)

	" Go to first line and remove it (empty line)
	execute "normal! 1Gdd"

	" Restore default register
	let @@ = saved_unnamed_register
endfunction

function! s:GetManDumpInNewWindow(word)
	vnew
	setlocal filetype=man_output buftype=nofile noswapfile
	execute "read !man -P cat ".a:word
	execute "normal! 1Gdd"
endfunction

" Command
command! -nargs=1 ViewManpage call <SID>GetManDumpInNewWindow(<args>)

" Operator for normal mode
nnoremap <leader>mp :set operatorfunc=<SID>ManOperator<cr>g@

" Operator for visual mode
vnoremap <leader>mp :<c-u>call <SID>ManOperator(visualmode())<cr>
