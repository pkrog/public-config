" Vim syntax file
" Language:     Parse::RecDescent (Perl module)
" Maintainer:   Flavio Poletti <flavio [you know what] polettix.it>
" Last Change:  2007 October 30
" Location:     none at the moment :)

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
	syntax clear
elseif exists("b:current_syntax")
	finish
endif

syn match prdDefinedAs /:/
syn region prdComment start=/#/ end=/$/
syn region prdDoubleString start=/"/ skip=/\\[\\"]/ end=/"/
syn region prdSingleString start=/'/ skip=/\\'/ end=/'/
syn match prdIdentifier /[a-zA-Z_][a-zA-Z0-9_]*/
syn region prdRegex start="m\?/"hs=e+1 skip="\\/" end="/[gmsxi]\?"he=s-1
syn include @Perl syntax/perl.vim
syn region prdCodeBlock start=/{/ end=/}/ contains=prdCodeBlock,@Perl

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_yacc_syn_inits")
	if version < 508
		let did_yacchdl_syn_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif

	HiLink prdDefinedAs    Operator
	HiLink prdComment      Comment
	HiLink prdDoubleString String
	HiLink prdSingleString String
	HiLink prdIdentifier   Keyword
	HiLink prdRegex        String

	delcommand HiLink
endif

let b:current_syntax = "parse_recdescent"
