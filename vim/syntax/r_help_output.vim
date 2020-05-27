if exists("b:current_syntax")
	finish
endif

let b:inside_r_help_output=1

syntax match rhelpOutputSection /\v^[A-Z][a-z]+:/
syntax match rhelpOutputTitle   /\v^.*R Documentation$/
syntax match rhelpOutputArg     /\v^ +[A-Za-z0-9.]+:/
syntax match rhelpOutputCode    /\v‘[^’]+’/

highlight link rhelpOutputTitle    Title
highlight link rhelpOutputSection  Section
highlight link rhelpOutputArg      Option
highlight link rhelpOutputCode     Code

call AddEmbeddedSyntaxHighlighting('r', '^Examples:$', '\%$', 'rhelpOutputSection')

let b:current_syntax = "r_help_output"
