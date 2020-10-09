" Usage
" <leader>ol in visual mode for opening selected link.
" <leader>ol in normal mode to open link under cursor.

" Set default values for global variables
if ! exists("g:view_link_cmd")
	let g:view_link_cmd = "lynx"
endif
if ! exists("g:view_link_flags")
	let g:view_link_flags = "-dump -list_inline -width=1024"
endif
if ! exists("g:view_link_filetype")
	let g:view_link_filetype = "lynx_output"
endif
if ! exists("g:view_link_chars")
	let g:view_link_chars = 'A-Za-z0-9_:/.?=-'
endif

" Operator
function! s:ViewLinkOperator(type)

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

	" Print web page output
	call DumpWebpage(@@)

	" Restore default register
	let @@ = saved_unnamed_register

endfunction

" Open window and write output of browser into it.
function! DumpWebpage(link)

	" Backup default register
	let saved_unnamed_register = @@

	if &filetype ==# g:view_link_filetype
		" Clean current buffer
		execute "normal! 1GVGd"
	else
		" Open new buffer
		vnew
		execute 'setlocal filetype='.g:view_link_filetype.' buftype=nofile noswapfile'
	endif

	" Insert browser output
	execute "read !".g:view_link_cmd.' '.g:view_link_flags.' '.shellescape(a:link)

	" Remove empty lines at start of buffer
	execute "normal! 1G"
	if match(getline('.'), "^\s*$") >= 0
		execute "normal! V/^.\<cr>kd"
	endif

	" Insert link visited in first line
	let @@ = a:link
	execute "normal! 1GOPage link: \<esc>po\<esc>"

	" Restore default register
	let @@ = saved_unnamed_register

endfunction

" View link under cursor
function! s:ViewLinkUnderCursor()

	" Backup values
	let saved_unnamed_register = @@
	let saved_cursor = getcurpos()

	" Select
	execute "normal! ?\\vhttps\\?://\<cr>v/\\v[^".g:view_link_chars."]+\<cr>hy"

	" Restore cursor position
	call setpos('.', saved_cursor)

	" Print web page output
	call DumpWebpage(@@)

	" Restore unnamed register
	let @@ = saved_unnamed_register

endfunction

" View link command
command! -nargs=1 ViewLink call DumpWebpage(<args>)

" Operator for normal mode
"nnoremap <leader>gw :set operatorfunc=<SID>ViewLinkOperator<cr>g@

" Operator for opening selected link in visual mode
vnoremap <leader>ol :<c-u>call <SID>ViewLinkOperator(visualmode())<cr>

" Operator for opening link under cursor in normal mode
nnoremap <leader>ol :call <SID>ViewLinkUnderCursor()<cr>
"nnoremap <leader>ol :execute "!tmux new-window lynx"<cr><cr>
" select link under cursor:
" l?http<cr>:nohlsearch<cr>v/[^ )]<cr>
"
" [Learn Vimscript the Hard Way](https://learnvimscriptthehardway.stevelosh.com/).
