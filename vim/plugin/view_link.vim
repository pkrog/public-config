if !exists("g:view_link_cmd")
	let g:view_link_cmd = "lynx"
endif
if !exists("g:view_link_chars")
	let g:view_link_chars = "A-Za-z0-9:/"
endif

function! s:SelectLinkUnderCursor()
	let saved_cursor = getcurpos()

	echom g:view_link_chars
	execute "normal! ?[^" . g:view_link_chars . ']'
	execute "normal! /[^" . g:view_link_chars . ']'

	call setpos('.', saved_cursor)
endfunction

" Command
command! -nargs=1 ViewLink call <SID>GetLynxDumpInNewWindow(<args>)

function! s:GetLynxDumpInNewWindow(link)
	vnew
	setlocal filetype=lynx_output buftype=nofile noswapfile
	execute "read !lynx -dump -list_inline -width=1024 ".a:link
	execute "normal! 1Gdd"
endfunction

" Open Lynx
nnoremap <leader>ol :call <SID>SelectLinkUnderCursor()<cr>
"nnoremap <leader>ol :execute "!tmux new-window lynx"<cr><cr>
" select link under cursor:
" l?http<cr>:nohlsearch<cr>v/[^ )]<cr>
"
" [Learn Vimscript the Hard Way](https://learnvimscriptthehardway.stevelosh.com/).
