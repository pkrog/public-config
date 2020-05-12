" Select some text under cursor and copy it to the unnamed register
function! SelectAndCopyAtCursorPos(chars)

	let l:selected = 0

	" Backup cursor position
	let l:saved_cursor = getcurpos()

	let l:cur_char = matchstr(getline('.'), '\%' . col('.') . 'c.')
	if l:cur_char =~? "^\\v[".a:chars."]$"
		" Search backward for first good char
		if col('.') != 1
			let l:search_start = "?\\v(^|[^".a:chars."])\<cr>"
			execute "normal! ".l:search_start
		endif

		" Search forward for first good char
		let l:cur_char = matchstr(getline('.'), '\%' . col('.') . 'c.')
		if l:cur_char !~? "^\\v[".a:chars."]$"
			let l:search_first_good_char = "/\\v[".a:chars."]\<cr>"
			execute "normal! ".l:search_first_good_char
		endif

		" Search for end
		let l:search_end = "/\\v([^".a:chars."]+|$)\<cr>"
		execute "normal! v".l:search_end."hy"

		let l:selected = 1
	endif

	" Restore cursor position
	call setpos('.', l:saved_cursor)

	return l:selected
endfunction

