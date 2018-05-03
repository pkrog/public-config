" vi: tw=0 ts=4 sw=4
" To reload: colorscheme darkscheme

set background=dark
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "darkscheme16"

"******************
" TEXT & COMMENTS *
"******************

hi Normal                   ctermfg=white       cterm=NONE
"hi Normal                   term=NONE       ctermfg=white       ctermbg=black       cterm=NONE
hi Comment                  term=underline  ctermfg=green       ctermbg=black       cterm=NONE
hi SpecialComment           term=underline  ctermfg=green       ctermbg=black       cterm=NONE
hi Special                  term=NONE       ctermfg=cyan        ctermbg=black       cterm=NONE
hi SpecialChar              term=NONE       ctermfg=yellow      ctermbg=black       cterm=NONE
hi Operator                 term=NONE       ctermfg=yellow      ctermbg=black       cterm=NONE
hi Delimiter                term=NONE       ctermfg=yellow      ctermbg=black       cterm=NONE
hi Label                    term=NONE       ctermfg=yellow      ctermbg=black       cterm=NONE
hi Identifier               term=NONE       ctermfg=yellow      ctermbg=black       cterm=NONE
hi Function                 term=NONE       ctermfg=yellow      ctermbg=black       cterm=NONE
hi Type                     term=NONE       ctermfg=blue        ctermbg=black       cterm=NONE
hi StorageClass             term=bold       ctermfg=magenta     ctermbg=black       cterm=NONE
hi Typedef                  term=bold       ctermfg=magenta     ctermbg=black       cterm=NONE
hi Structure                term=bold       ctermfg=magenta     ctermbg=black       cterm=NONE
hi Statement                term=bold       ctermfg=red         ctermbg=black       cterm=NONE
hi Repeat                   term=bold       ctermfg=red         ctermbg=black       cterm=NONE
hi Conditional              term=bold       ctermfg=red         ctermbg=black       cterm=NONE
hi Exception                term=bold       ctermfg=red         ctermbg=black       cterm=NONE
hi PreProc                  term=bold       ctermfg=brown       ctermbg=black       cterm=NONE
hi PreCondit                term=bold       ctermfg=brown       ctermbg=black       cterm=NONE
hi Include                  term=bold       ctermfg=brown       ctermbg=black       cterm=NONE
hi Define                   term=bold       ctermfg=brown       ctermbg=black       cterm=NONE
hi Macro                    term=bold       ctermfg=brown       ctermbg=black       cterm=NONE
hi Constant                 term=NONE       ctermfg=cyan        ctermbg=black       cterm=NONE
hi Boolean                  term=NONE       ctermfg=cyan        ctermbg=black       cterm=NONE
hi Number                   term=NONE       ctermfg=cyan        ctermbg=black       cterm=NONE
hi Float                    term=NONE       ctermfg=blue        ctermbg=black       cterm=NONE
hi Character                term=NONE       ctermfg=cyan        ctermbg=black       cterm=NONE
hi String                   term=NONE       ctermfg=darkgreen   ctermbg=black       cterm=NONE
hi Title                    term=bold       ctermfg=red         ctermbg=black       cterm=NONE
hi Folded                   term=NONE       ctermfg=blue        ctermbg=black       cterm=NONE
hi fortranTab               term=NONE       ctermfg=white       ctermbg=darkgrey    cterm=NONE
hi SpecialKey               term=NONE       ctermfg=white       ctermbg=darkgrey    cterm=NONE
hi markdownCodeDelimiter    term=NONE       ctermfg=blue        ctermbg=black       cterm=NONE
hi markdownCode             term=NONE       ctermfg=blue        ctermbg=black       cterm=NONE
hi markdownCodeBlock        term=NONE       ctermfg=blue        ctermbg=black       cterm=NONE

hi Todo                     term=reverse    ctermfg=black   ctermbg=yellow      cterm=NONE
hi Error                    term=reverse    ctermfg=white   ctermbg=red         cterm=NONE

hi StatusLine               term=reverse    ctermfg=black   ctermbg=cyan        cterm=NONE
hi StatusLineNC             term=reverse    ctermfg=black   ctermbg=lightgrey   cterm=NONE
hi Directory                term=NONE       ctermfg=blue    ctermbg=black       cterm=NONE 
hi NonText                  term=NONE       ctermfg=blue    ctermbg=black       cterm=NONE
hi Visual                   term=reverse    ctermfg=black   ctermbg=lightgrey   cterm=NONE
hi ModeMsg                  term=NONE       ctermfg=brown   ctermbg=black       cterm=NONE
hi ErrorMsg                 term=NONE       ctermfg=brown   ctermbg=black       cterm=NONE
hi WarningMsg               term=NONE       ctermfg=brown   ctermbg=black       cterm=NONE
hi LineNr                   term=NONE       ctermfg=white   ctermbg=black       cterm=NONE
hi Question                 term=NONE       ctermfg=magenta ctermbg=black       cterm=NONE
hi Search                   term=reverse    ctermfg=black   ctermbg=yellow      cterm=NONE
hi IncSearch                term=reverse    ctermfg=black   ctermbg=yellow      cterm=NONE

hi SpellBad                 term=reverse    ctermfg=black   ctermbg=red         cterm=NONE
hi SpellLocal               term=reverse    ctermfg=black   ctermbg=red         cterm=NONE
hi SpellCap                 term=reverse    ctermfg=black   ctermbg=red         cterm=NONE
hi SpellRare                term=reverse    ctermfg=black   ctermbg=red         cterm=NONE

hi Ignore                   term=reverse    ctermfg=black   ctermbg=green       cterm=NONE
hi Cursor                   term=reverse    ctermfg=black   ctermbg=green       cterm=NONE
hi MoreMsg                  term=reverse    ctermfg=black   ctermbg=green       cterm=NONE

" DESCRIPTIONS
" Directory     VIM. Directory inside Netrw file system explorer.
" Error         VIM. Used for instance when there's a mismatch in braces or parenthesis.
" ErrorMsg      VIM error message when searching.
" IncSearch     VIM incremental search (command `se incsearch`).
" LineNr        VIM line numbers (command `set nu`).
" ModeMsg       VIM mode information (Insert, Visual, etc.).
" NonText       VIM end of file. Markdown code block markers.
" Question      VIM input required (press ENTER, or answer question).
" Search        VIM highlight search (command `set hlsearch`).
" SpellLocal    VIM. Words that are not used in one region but are used in another region are highlighted with SpellLocal.
" Visual        VIM text selection.
" WarningMsg    VIM warning message. For instance when there is no more search match left and warn that search will be restarted from the top.
