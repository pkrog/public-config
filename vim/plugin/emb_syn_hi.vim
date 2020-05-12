function! AddEmbeddedSyntaxHighlighting(filetype, start, end, textSnipHl) abort

	" Back up some variables and options
	if exists('b:current_syntax')
		" Some syntax files (e.g. cpp.vim) do nothing if b:current_syntax is defined.
		let l:main_syntax = b:current_syntax
		let l:backed_up_syntax = b:current_syntax
		unlet b:current_syntax
	else
		let l:main_syntax = 'text'
	endif
	if exists('&l:foldmethod')
		let l:foldmethod_bkp = &l:foldmethod
	endif

	let ft=toupper(a:filetype)
	let group = l:main_syntax . 'Group'.ft

	" Load syntax highlighting of language to embed
	let syn_cmd = 'syntax include @'.group.' syntax/'.a:filetype.'.vim'
	execute syn_cmd
	try
		let syn_cmd = 'syntax include @'.group.' after/syntax/'.a:filetype.'.vim'
		execute syn_cmd
	catch
		" No after syntax file
	endtry

	" Restore variables and options
	if exists('l:backed_up_syntax')
		let b:current_syntax = l:backed_up_syntax
	else
		unlet b:current_syntax
	endif
	if exists('l:foldmethod_bkp')
		let &l:foldmethod = l:foldmethod_bkp
	endif

	" Create syntax region
	let syn_cmd = 'syntax region '.s:main_syntax.'Emb'.ft
	let syn_cmd .= ' matchgroup='.a:textSnipHl
	let syn_cmd .= ' keepend'
	let syn_cmd .= ' start="'.a:start.'" end="'.a:end.'"'
	let syn_cmd .= ' contains=@'.group
	execute syn_cmd

endfunction
