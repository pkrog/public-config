" General settings {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set nocompatible " Necessary in order to set `filetype on`
filetype plugin indent on " Enable loading of ftplugin/* files.

set encoding=utf-8
set clipboard=unnamed
colorscheme darkscheme
set hlsearch " highlight search
set incsearch " use incremental search
"set fileformat=unix --> TODO set it on some file types ?
set backspace=2 " Enable backspace.
set guioptions=
set autowrite
set splitright
set splitbelow

" Modeline
set modeline " Enable modeline.
set modelines=5 " Number of lines checked.

" 256 colors
if !has('gui_running')
	set t_Co=256
endif

" Indentation
set noexpandtab
set copyindent
set preserveindent
set softtabstop=0
set shiftwidth=4
set tabstop=4
set cindent
set cinoptions=(0,u0,U0)

" Swap files
"set directory^=~/.vim/swp//
"set backupdir^=~/.vim/bkp//

" Paths
set path+=~/dev
set path+=~/dev/public-notes
set path+=~/dev/public-notes/dev
set path+=~/dev/public-notes/cuisine
set path+=~/dev/public-notes/misc
set path+=~/dev/private-notes
set path+=~/dev/cea_notes
set path+=~/private
set path+=~/private/dev/cea_notes
set path+=~/private/dev/private-notes

" Function for getting syntax highlithing group at current cursor place.
function! SyntaxItem()
  return join(map(synstack(line("."), col(".")), 'synIDattr(v:val, "name")'))
endfunction

" Tabs and whitespaces highlighting {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Print tabs
set listchars=tab:\|\ 
set list
" Override Make filetype definition:
autocmd FileType make set listchars=tab:\|\ 

" Whitespaces at end of line
"au BufRead,BufNewFile * match Debug /\s\+$/

" Netrw {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:netrw_liststyle = 3 " Tree view
let g:netrw_banner = 0 " Suppress banner
"let g:netrw_browse_split = 4
"let g:netrw_altv = 1
"let g:netrw_winsize = 25
"augroup ProjectDrawer
"	autocmd!
"	autocmd VimEnter * :Vexplore
"augroup END

" Make {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

autocmd syntax make set list
autocmd syntax make set listchars=tab:\ \ 
set autowrite

" Mail {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"autocmd syntax mail setl formatoptions+=a

" Perl {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:perl_fold = 1

" Python {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

au BufNewFile,BufRead *.py setlocal tabstop=4 softtabstop=4 shiftwidth=4 textwidth=0 colorcolumn=80 expandtab autoindent fileformat=unix
" TODO How to enable foldmethod=indent in Python?

" R {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" TODO IMPORTANT How to highlight `<-` in other languages so when I get
" confused between `=` and R `<-`.

" Coloring R output
let g:rout_follow_colorscheme = 1
let g:Rout_more_colors = 1
let g:R_assign = 0 " Disable replacement of "_" by " <- ".

" Enable auto syntax folding
let r_syntax_folding = 1

if has("autocmd")
  if v:version > 701
	au BufRead,BufNewFile *.R setlocal tabstop=4 expandtab colorcolumn=80 textwidth=0

	" Override syntax default highlighting of numbers. Numbers ([0-9]+) are float in R, not integers.
	au Syntax r hi! def link rNumber Float
  endif
endif


" RecDescent parsing {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

au BufRead,BufNewFile *.prd set filetype=parse_recdescent

" File autocompletion {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set wildmenu
set wildmode=longest:full,full
set wildignore+=*.a,*.o
set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.png
set wildignore+=.DS_Store,.git,.hg,.svn
set wildignore+=*~,*.swp,*.tmp

" Markdown / Pandoc {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" https://github.com/vim-pandoc/vim-pandoc
" let g:pandoc#filetypes#handled = ["pandoc", "markdown"]
" let g:pandoc#filetypes#pandoc_markdown = 1
" let g:pandoc#syntax#codeblocks#embeds#langs = ['vim', 'bash=sh', 'perl', 'r']

" https://github.com/masukomi/vim-markdown-folding
"if has('autocmd')
"	autocmd FileType markdown set foldexpr=NestedMarkdownFolds()
"endif

" https://github.com/plasticboy/vim-markdown

" Loading packages earlier {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if v:version >= 800
	packloadall
endif

" Miscellaneous {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Redefine what is selected under cursor when calling `gx`.
"let g:netrw_gx="<cWORD>"
"let g:netrw_browsex_viewer="open"
let g:netrw_special_syntax=1

set modeline
set modelines=5
set backspace=2 "enable backspace
set guioptions=

" Status line (bottom bar)
set laststatus=2
if ! has('gui_running')
	set t_Co=256
endif
if exists('g:loaded_lightline')
	" Use Light line plugin.
	" See https://github.com/itchyny/lightline.vim.
	if exists('g:loaded_fugitive')
		let g:lightline = {
		      \ 'colorscheme': 'powerline',
		      \ 'active': {
		      \   'left': [ [ 'mode', 'paste' ],
		      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
		      \ },
		      \ 'component_function': {
		      \   'gitbranch': 'fugitive#head'
		      \ },
		      \ }
	endif
elseif executable("powerline")
	python3 from powerline.vim import setup as powerline_setup
	python3 powerline_setup()
	python3 del powerline_setup
else
	set statusline=%n:%<%f%m%=%{getcwd()},%l,%c-%P%Y%R
endif

function! SyntaxItem()
"	  return synIDattr(synID(line("."),col("."),1),"name")
  return join(map(synstack(line("."), col(".")), 'synIDattr(v:val, "name")'))
endfunction
" Usage: run `echo SyntaxItem()` when cursor is on the wanted text.

" Display syntax highlighting group in status bar
"set statusline+=%{SyntaxItem()}

" Error formats {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set errorformat=

" R testthat {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"   8. .self$.parseDbLinks(parsed.content) R/KeggCompoundEntry.R:37:4
set errorformat+=\ %#%n.\ %m\ %f:%l:%c%.%#

" Failure (test_011_mass_fcts.R:33:5): closeMatchPpm() works correctly.
set errorformat+=Failure\ (%f:%l:%c)\ %m

" ── 1. Error: annotateMzValues() works correctly. (@BiodbCompounddbConn.R#121)  ─
set errorformat+=%.%#.\ Error:\ %m\ (@%f#%l)\ %.%#

" ????
"   Attention ! Do not parse Maven compiling message:
"   [INFO] Finished at: 2021-01-06T16:33:06+01:00
"set errorformat+=%m\ at\ %f:%l        " R testthat error

" R BiocCheck {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set errorformat+=%m\ in\ %f\ (line\ %l\\,\ column\ %c) " BiocCheck error
set errorformat+=%m\ (%f\\,\ line\ %l):\ %.%#  " BiocCheck error
set errorformat+=%f\ (line\ %l\\,\ column\ %c) " BiocCheck error
set errorformat+=%f\ (line\ %l) " BiocCheck error

" PHP {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" .PHP Fatal error:  Constant expression contains invalid operations in
" /home/pr228844/dev/exhalobase/site/src/DbAcqFile.php on line 37
set errorformat+=.PHP\ Fatal\ error:\ %#%m\ in\ %f\ on\ line\ %l " PHP

set errorformat+=PHP\ Parse\ error:\ %#%m\ in\ %f\ on\ line\ %l " PHP

" Perl {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set errorformat+=%m\ at\ %f\ line\ %l. " Perl

" set errorformat=%-GDEBUG%.%#                           " Ignore all debug messages, starting by DEBUG.
"set errorformat+=%-G[%.%#INFO%.%#]%.%#                  " Maven info message. We ignore all of them.
" set errorformat+=%A\ %#[javac]\ %f:%l:\ %m,%-Z\ %#[javac]\ %p^,%-C%.%# " Ant/Java. Put at beginning of list, otherwise another takes precedence.
" 
" set errorformat+=Error\ in\ %f\ (line\ %l)      " Matlab    Note that it requires that the Matlab output be filtered first in order to add the .m extension to the file.

" R
"set errorformat^=%.%#[exec]%.%#\ %f:%l:%c:\ %m    " R error when run with devtools package
"set errorformat+=%.%#(from\ %f#%l)%m  " R error when run with devtools package

" Maven {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set errorformat+=[%.%#WARNING%.%#]\ %f:[%l\\,%c]\ %m    " Maven warning message. `%.%#` is here to catch color codes.

" [ERROR] /home/pr228844/private/dev/anmf/src/main/java/fr/cea/anmf/DiagPicturesPanel.java:[62,15] mousePressed(java.awt.event.MouseEvent) in fr.cea.anmf.DiagPicturesPanel cannot implement mousePressed(java.awt.event.MouseEvent) in java.awt.event.MouseListener
" [ERROR]   attempting to assign weaker access privileges; was public
set errorformat+=[%.%#ERROR%.%#]\ %f:[%l\\,%c]\ %m      " `%.%#` is here to catch color codes.

""set errorformat-=%f\\|%l\\|\ %m
""set errorformat+=%.%#\ %f\\|%l\\|\ %m        " ant/Java
""set errorformat+=%f\\|%l\\|\ %m


" Generic {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Too much generic
"set errorformat+=%f:%l:\ %m " Custom file read with cfile
"set errorformat+=%f:%l\ %m " BiocCheck error
"set errorformat+=%m\ in\ %f\ on\ line\ %l " PHP syntax error
"set errorformat+=%f:%l " line error in PHP Unit stack print

"" PHP
"" 1) DbAccessTest::test_01_resetDb
"" ParseError: syntax error, unexpected ';', expecting ']'
""
"" /home/pr228844/dev/exhalobase/site/src/ExhalobaseAccess.php:152
"" /home/pr228844/dev/exhalobase/tests/20_DbAccessTest.php:44
"set errorformat+=%f:%l
"
"" ant/Java
""let fmt = '%A\ %#[javac]\ %f:%l:\ %m,'
""	\ . '%A\ %#[aapt]\ %f:%l:\ %m,'
""	\ . '%-Z\ %#[javac]\ %p^,'
""	\ . '%C\ %#[javac]\ %m,'
""	\ . '%-C%.%#'
""execute 'set errorformat+=' . fmt

" Spell checking {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

for lang in ['fr', 'en', 'it']
	let wordsfile = expand("~/.vim/spell/" . lang . ".utf-8.add")
	let splfile = wordsfile . ".spl"
	if (! filereadable(splfile)) || getftime(splfile) < getftime(wordsfile)
		exec "silent mkspell! " . wordsfile 
	endif
endfor
command! -nargs=1 SpellCheck setlocal spelllang=<args> | set spell

" Complete line {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Copy same character from cursor pos up to colorcolumn value (why not textwidth?)
" Gives trouble with file system explorer netrw.

" function CompleteLine(c)
" 	exec 'normal '.(&cc - strlen(getline('.'))).'A'.nr2char(a:c)
" endfunction

" nnoremap <expr> m ':call CompleteLine('.getchar().")\<CR>"

" Key mappings {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Correct ambiguous abbreviation on schroeder installation of vim (ambiguity between ELP and Explore).
cabbrev E Explore

let mapleader = ","
let maplocalleader = "\\"

" Edit and source .vimrc
nnoremap <leader>ev :rightbelow vsplit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

" Run tests
nnoremap <leader>mt :make test<cr><cr>:copen<cr>

"nnoremap <leader>wh :!lynx
"http://us2.php.net/^R^W\#function.^R^W<cr>

" Remove white spaces at end of line
nnoremap <leader>dw V:s/[[:space:]]\+$//<cr>

" Quote current word in normal mode
nnoremap <leader>" viw<esc>a"<esc>bi"<esc>lel

" Clipboard copy & paste
" TODO

inoremap jk <esc>
inoremap jj <esc>:w<cr>
"inoremap <esc> <nop>

" Normal mode
"    a  append
"    A  append at end of line
"nmap e  :tabedit
"nmap f  :tabfind
"    h  cursor left
"    i  insert mode
"    I  go to start of text in the current line and enter insert mode.
"    j  cursor down
"    k  cursor up
"    l  cursor right
"    p  paste
"    P  paste
"    r  replace current character
"    R  replace mode
"    s  DISABLED delete current character and go into insert mode.
nmap se :SpellCheck en<cr>
nmap sf :SpellCheck fr<cr>
nmap si :SpellCheck it<cr>
nmap sn :se nospell<cr>
"    S  delete current line and go into insert mode.
"nmap t  gt
nnoremap <tab> gt
nnoremap <s-tab> gT
"nmap T  gT
"    x  cut
"    zb Put cursor line at bottom
"    zc Close a fold.
nmap zh :noh<cr>
nmap zj :%!python -m json.tool<cr>
"    zm Close all folds by one level.
"    zo Open a fold.
"    zr Open all folds by one level.
nmap zs :source $MYVIMRC<cr>
"    zt Put cursor line at top
nmap zT :TabooRename
"    zz center cursor line
"    C-a increment number
nmap <C-z> <C-a>
"    C-x decrement number

" TODO create a key binding for editing file under cursor and jumping to first occurence of a specified word.
" Format: `some/path/to/a/file.txt#chapter1`.
" Use isfname to know which character are allowed in a filename.

" Syntax on {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

syntax on " Set last because of csv.vim plugin

" Vimscript file settings {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

augroup filetype_vim
	autocmd!
	autocmd FileType vim setlocal foldmethod=marker
augroup END

" Highlighting of TODO, URGENT, NOTE, etc {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

augroup syntax_notes
	autocmd!
	autocmd Syntax * syntax keyword allTodo TODO TOREAD TOEXPLAIN TOREMOVE TOREFACTOR TOREVIEW WARNING containedin=.*Comment.* contained
	autocmd Syntax * syntax keyword allNote NOTE INFO IDEA XXX DONE containedin=.*Comment.* contained
	autocmd Syntax * syntax keyword allUrgent URGENT DEPRECATED BROKEN BUG IMPORTANT ERROR HACK FIXME DISABLED containedin=.*Comment.* contained
	autocmd Syntax * highlight link allTodo Todo
	autocmd Syntax * highlight link allNote Debug
	autocmd Syntax * highlight link allUrgent Error
augroup END

" Included syntax highlighting {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

autocmd Syntax xml call SyntaxRange#Include('^.*@@@BEGIN_RST@@@.*$', '^.*@@@END_RST@@@.*$', 'rst', 'NonText') " reStructuredText
autocmd Syntax xml call SyntaxRange#Include('^.*@@@BEGIN_CHEETAH@@@.*$', '^.*@@@END_CHEETAH@@@.*$', 'cheetah', 'NonText')
autocmd Syntax sh call SyntaxRange#Include('^.*@@@BEGIN_SQL@@@.*$', '^.*@@@END_SQL@@@.*$', 'sql', 'NonText')
autocmd Syntax sh call SyntaxRange#Include('^.*@@@BEGIN_PYTHON@@@.*$', '^.*@@@END_PYTHON@@@.*$', 'python', 'NonText')
autocmd Syntax r call SyntaxRange#Include('^.*@@@BEGIN_CPP@@@.*$', '^.*@@@END_CPP@@@.*$', 'cpp', 'NonText')
autocmd Syntax sh call SyntaxRange#Include('^.*@@@BEGIN_DIRCOLORS@@@.*$', '^.*@@@END_DIRCOLORS@@@.*$', 'dircolors', 'NonText')
autocmd Syntax sh call SyntaxRange#Include('^.*@@@BEGIN_JAVASCRIPT@@@.*$', '^.*@@@END_JAVASCRIPT@@@.*$', 'javascript', 'NonText') " Javascript inside shell
