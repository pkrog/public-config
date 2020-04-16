function! AddEmbeddedSyntaxHighlighting(filetype, start, end, textSnipHl) abort

	let ft=toupper(a:filetype)
	let group='textGroup'.ft

	" Back up and disable current syntax definition.
	" Some syntax files (e.g. cpp.vim) do nothing if b:current_syntax is defined.
	if exists('b:current_syntax')
		let s:current_syntax=b:current_syntax
		unlet b:current_syntax
	endif

	" Load syntax highlighting of language to embed
	let syn_cmd = 'syntax include @'.group.' syntax/'.a:filetype.'.vim'
	execute syn_cmd
	try
		let syn_cmd = 'syntax include @'.group.' after/syntax/'.a:filetype.'.vim'
		execute syn_cmd
	catch
		" No after syntax file
	endtry

	" Restore current syntax
	if exists('s:current_syntax')
		let b:current_syntax=s:current_syntax
	else
		unlet b:current_syntax
	endif

	" Create syntax region
	let syn_cmd = 'syntax region textSnip'.ft
	let syn_cmd .= ' matchgroup='.a:textSnipHl
	let syn_cmd .= ' keepend'
	let syn_cmd .= ' start="'.a:start.'" end="'.a:end.'"'
	let syn_cmd .= ' contains=@'.group
	execute syn_cmd

endfunction
