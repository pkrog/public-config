if has('autocmd')
	augroup filetype_markdown
		autocmd!
		autocmd FileType markdown setlocal foldmethod=expr
	augroup END
endif
