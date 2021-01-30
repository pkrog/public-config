" vi: tw=0 ts=4 sw=4 ft=vim_colors
" To reload: colorscheme darkscheme

set background=dark
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "darkscheme"

"******************
" TEXT & COMMENTS *
"******************

hi Normal                   term=NONE       ctermfg=255             cterm=NONE
hi Comment                  term=underline  ctermfg=154             cterm=NONE
hi SpecialComment           term=underline  ctermfg=34              cterm=NONE
hi Special                  term=NONE       ctermfg=117             cterm=NONE
hi SpecialChar              term=NONE       ctermfg=220             cterm=NONE
hi Operator                 term=NONE       ctermfg=220             cterm=NONE
hi Delimiter                term=NONE       ctermfg=220             cterm=NONE
hi Label                    term=NONE       ctermfg=220             cterm=NONE
hi Identifier               term=NONE       ctermfg=220             cterm=NONE
hi Function                 term=NONE       ctermfg=220             cterm=NONE
hi Type                     term=NONE       ctermfg=69              cterm=NONE
hi StorageClass             term=bold       ctermfg=198             cterm=NONE
hi Typedef                  term=bold       ctermfg=171             cterm=NONE
hi Structure                term=bold       ctermfg=171             cterm=NONE
hi Statement                term=bold       ctermfg=196             cterm=NONE
hi Repeat                   term=bold       ctermfg=196             cterm=NONE
hi Conditional              term=bold       ctermfg=196             cterm=NONE
hi Exception                term=bold       ctermfg=196             cterm=NONE
hi PreProc                  term=bold       ctermfg=208             cterm=NONE
hi PreCondit                term=bold       ctermfg=208             cterm=NONE
hi Include                  term=bold       ctermfg=208             cterm=NONE
hi Define                   term=bold       ctermfg=208             cterm=NONE
hi Macro                    term=bold       ctermfg=208             cterm=NONE
hi Constant                 term=NONE       ctermfg=81              cterm=NONE
hi Boolean                  term=NONE       ctermfg=81              cterm=NONE
hi Number                   term=NONE       ctermfg=81              cterm=NONE
hi Float                    term=NONE       ctermfg=33              cterm=NONE
hi Character                term=NONE       ctermfg=81              cterm=NONE
hi String                   term=NONE       ctermfg=36              cterm=NONE
hi Folded                   term=NONE       ctermfg=51              cterm=NONE
hi fortranTab               term=NONE       ctermfg=255 ctermbg=236 cterm=NONE
hi SpecialKey               term=NONE       ctermfg=31               cterm=NONE
hi Folded                   term=NONE       ctermfg=255 ctermbg=NONE cterm=NONE

hi netrwCompress    term=NONE       ctermfg=220             cterm=NONE
hi netrwData        term=NONE       ctermfg=43              cterm=NONE
hi netrwHdr         term=NONE       ctermfg=213             cterm=NONE
hi netrwLex         term=NONE       ctermfg=213             cterm=NONE
hi netrwYacc        term=NONE       ctermfg=213             cterm=NONE
hi netrwLib         term=NONE       ctermfg=34              cterm=NONE
hi netrwObj         term=NONE       ctermfg=34              cterm=NONE
hi netrwTilde       term=NONE       ctermfg=34              cterm=NONE
hi netrwTmp         term=NONE       ctermfg=34              cterm=NONE
hi netrwTags        term=NONE       ctermfg=34              cterm=NONE
hi netrwDoc         term=NONE       ctermfg=202             cterm=NONE
hi netrwSymLink     term=NONE       ctermfg=154             cterm=NONE
hi netrwMakefile    term=NONE       ctermfg=92              cterm=NONE

hi Title           term=bold       ctermfg=196             cterm=NONE
hi Section         term=underline  ctermfg=208             cterm=NONE
hi Option          term=underline  ctermfg=220             cterm=NONE
hi Reference       term=underline  ctermfg=33              cterm=NONE
hi Code            term=underline  ctermfg=69              cterm=NONE

hi markdownCodeDelimiter    term=NONE       ctermfg=34              cterm=NONE
hi markdownCode             term=NONE       ctermfg=69              cterm=NONE
hi markdownCodeBlock        term=NONE       ctermfg=69              cterm=NONE

hi LynxOutputPageTitle      term=underline  ctermfg=196 ctermbg=220 cterm=NONE
hi LynxOutputHttpLink       term=underline  ctermfg=33              cterm=NONE
hi LynxOutputLink           term=underline  ctermfg=220             cterm=NONE
hi LynxOutputBulletStar     term=bold       ctermfg=154             cterm=NONE
hi LynxOutputBulletPlus     term=NONE       ctermfg=208             cterm=NONE
hi LynxOutputSharp          term=bold       ctermfg=196             cterm=NONE

hi Todo                     term=reverse    ctermfg=0   ctermbg=11  cterm=NONE
hi Debug                    term=reverse    ctermfg=255 ctermbg=12  cterm=NONE
hi Error                    term=reverse    ctermfg=255 ctermbg=9   cterm=NONE

hi StatusLine               term=reverse    ctermfg=0   ctermbg=153 cterm=NONE
hi StatusLineNC             term=reverse    ctermfg=0   ctermbg=245 cterm=NONE
hi Directory                term=NONE       ctermfg=69              cterm=NONE 
hi NonText                  term=NONE       ctermfg=99              cterm=NONE
hi Visual                   term=reverse    ctermfg=0   ctermbg=240 cterm=NONE
hi ModeMsg                  term=NONE       ctermfg=150             cterm=NONE
hi ErrorMsg                 term=NONE       ctermfg=1               cterm=NONE
hi WarningMsg               term=NONE       ctermfg=130             cterm=NONE
hi LineNr                   term=NONE       ctermfg=255             cterm=NONE
hi Question                 term=NONE       ctermfg=171             cterm=NONE
hi Search                   term=reverse    ctermfg=0   ctermbg=220 cterm=NONE
hi IncSearch                term=reverse    ctermfg=0   ctermbg=76  cterm=NONE

hi SpellBad                 term=reverse    ctermfg=0   ctermbg=9   cterm=NONE
hi SpellLocal               term=reverse    ctermfg=0   ctermbg=120 cterm=NONE
hi SpellCap                 term=reverse    ctermfg=0   ctermbg=69  cterm=NONE
hi SpellRare                term=reverse    ctermfg=0   ctermbg=200 cterm=NONE

hi Ignore                   term=reverse    ctermfg=0   ctermbg=100 cterm=NONE
hi Cursor                   term=reverse    ctermfg=0   ctermbg=80  cterm=NONE
hi MoreMsg                  term=reverse    ctermfg=0   ctermbg=30  cterm=NONE

hi CSVColumnHeaderEven      term=NONE       ctermfg=40              cterm=NONE
hi CSVColumnHeaderOdd       term=reverse    ctermfg=208             cterm=NONE
hi CSVColumnEven            term=NONE       ctermfg=40              cterm=NONE
hi CSVColumnOdd             term=reverse    ctermfg=208             cterm=NONE

for bgcol in range(255, 0, -1)
	for fgcol in range(255, 0, -1)
		if bgcol != fgcol && (fgcol == 0 || fgcol == 255 || bgcol == 0 || bgcol == 255)
			exec 'hi ColorFg'.fgcol.'Bg'.bgcol.' term=NONE ctermfg='.fgcol.' ctermbg='.bgcol.' cterm=NONE'
		endif
	endfor
endfor

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
