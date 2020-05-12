" TODO Move to general script for all buffers
" Read special modeline
let modeline_number = search("vimvars:", "cn", 4)
if modeline_number != 0
	let line = getline(modeline_number)
	let vars = []
	call substitute(line, 'b:[a-z_]\+=[^ ]\+', '\=add(vars, submatch(0))', 'g')
	for var in vars
		execute "let " . var
	endfor
endif

" Set default embedded syntax
if ! exists("g:markdown_embedded_syntax")
	" Keys are tags and values are syntax names
	" ex.: {'apache':'', 'js':'javascript' }
	let g:markdown_embedded_syntax = {}
endif
if ! exists("b:markdown_embedded_syntax")
	let b:markdown_embedded_syntax = {}
endif

let s:embedded_syntax = extend(g:markdown_embedded_syntax, b:markdown_embedded_syntax)
for ktag in keys(s:embedded_syntax)
	let vsyn = s:embedded_syntax[ktag]
	if vsyn == ''
		let vsyn = ktag
	endif
	call AddEmbeddedSyntaxHighlighting(vsyn, '^```'.ktag.'$', '^```$', 'markdownCodeDelimiter')
endfor
