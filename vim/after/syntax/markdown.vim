" Set default embedded syntax
if ! exists("g:markdown_embedded_syntax")
	" Keys are tags and values are syntax names
	let g:markdown_embedded_syntax = {'apache':'', 'bash':'', 'c':'', 'cfg':'conf', 'conf':'', 'cpp':'', 'css':'', 'help':'', 'html':'', 'js':'javascript', 'javascript':'', 'perl':'', 'php':'', 'python':'', 'r':'', 'ruby':'', 'sh':'bash', 'sql':'', 'vim':'', 'xml':'', 'yaml':''}
endif

for ktag in keys(g:markdown_embedded_syntax)
	let vsyn = g:markdown_embedded_syntax[ktag]
	if vsyn == ''
		let vsyn = ktag
	endif
	call AddEmbeddedSyntaxHighlighting(vsyn, '^```'.ktag.'$', '^```$', 'markdownCodeDelimiter')
endfor
