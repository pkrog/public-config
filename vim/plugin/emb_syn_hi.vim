function! AddEmbeddedSyntaxHighlighting(filetype, start, end, textSnipHl) abort

	" Back up and disable current syntax definition.
	" Some syntax files (e.g. cpp.vim) do nothing if b:current_syntax is defined.
	if exists('b:current_syntax')
		let s:main_syntax = b:current_syntax
		let s:backed_up_syntax = b:current_syntax
		unlet b:current_syntax
	else
		let s:main_syntax = 'text'
	endif

	let ft=toupper(a:filetype)
	let group = s:main_syntax . 'Group'.ft

	" Load syntax highlighting of language to embed
	let syn_cmd = 'syntax include @'.group.' syntax/'.a:filetype.'.vim'
	execute syn_cmd
	try
		let syn_cmd = 'syntax include @'.group.' after/syntax/'.a:filetype.'.vim'
		execute syn_cmd
	catch
		" No after syntax file
	endtry

	" Restore backed up syntax
	if exists('s:backed_up_syntax')
		let b:current_syntax = s:backed_up_syntax
	else
		unlet b:current_syntax
	endif

	" Create syntax region
	let syn_cmd = 'syntax region '.s:main_syntax.'Emb'.ft
	let syn_cmd .= ' matchgroup='.a:textSnipHl
	let syn_cmd .= ' keepend'
	let syn_cmd .= ' start="'.a:start.'" end="'.a:end.'"'
	let syn_cmd .= ' contains=@'.group
	execute syn_cmd

endfunction
