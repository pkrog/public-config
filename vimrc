" vi: fdm=marker

" General settings {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set encoding=utf-8
set clipboard=unnamed
syntax on
colorscheme darkscheme
set hlsearch " highlight search
set fileformat=unix
set backspace=2 " Enable backspace.
set guioptions=

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
set directory^=~/.vim/swp//
set backupdir^=~/.vim/bkp//

" Paths
set path+=~/dev/public-notes
set path+=~/dev/private-notes

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
au BufRead,BufNewFile * match Debug /\s\+$/

" Included syntax highlighting {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

autocmd Syntax xml call SyntaxRange#Include('^.*@@@BEGIN_RST@@@.*$', '^.*@@@END_RST@@@.*$', 'rst', 'NonText') " reStructuredText
autocmd Syntax xml call SyntaxRange#Include('^.*@@@BEGIN_CHEETAH@@@.*$', '^.*@@@END_CHEETAH@@@.*$', 'cheetah', 'NonText')
autocmd Syntax sh call SyntaxRange#Include('^.*@@@BEGIN_SQL@@@.*$', '^.*@@@END_SQL@@@.*$', 'sql', 'NonText')
autocmd Syntax sh call SyntaxRange#Include('^.*@@@BEGIN_PYTHON@@@.*$', '^.*@@@END_PYTHON@@@.*$', 'python', 'NonText')
autocmd Syntax r call SyntaxRange#Include('^.*@@@BEGIN_CPP@@@.*$', '^.*@@@END_CPP@@@.*$', 'cpp', 'NonText')
autocmd Syntax sh call SyntaxRange#Include('^.*@@@BEGIN_DIRCOLORS@@@.*$', '^.*@@@END_DIRCOLORS@@@.*$', 'dircolors', 'NonText')
" Commented out Markdown syntax highlighting inside XML, because it clashes with reStructuredText:
"     Error detected while processing /usr/local/Cellar/vim/8.0.1200/share/vim/vim80/syntax/vim.vim:
"     line  791:
"     E403: syntax sync: line continuations pattern specified twice
"autocmd Syntax xml call SyntaxRange#Include('^.*@@@BEGIN_MARKDOWN@@@.*$', '^.*@@@END_MARKDOWN@@@.*$', 'markdown', 'NonText')

" Highlighting of TODO, URGENT, NOTE, etc {{{1

" FIXME When splitting a window, the highlighting disappears.
if has("autocmd")
  if v:version > 701
    autocmd Syntax * call matchadd('Debug', '\W\zs\(NOTE\|INFO\|IDEA\)')
    autocmd Syntax * call matchadd('Todo',  '\W\zs\(TODO\|FIXME\|XXX\|TOREAD\|TO READ\|REFACTOR\|TO \?REMOVE\)')
    autocmd Syntax * call matchadd('Error', '\W\zs\(URGENT\|DEPRECATED\|BUG\|ERROR\|IMPORTANT\)')
  endif
endif


" Make {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

autocmd syntax make set list
autocmd syntax make set listchars=tab:\ \ 
set autowrite

" Mail {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"autocmd syntax mail setl formatoptions+=a

" Markdown {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Enable markdown plugin foldmethod
filetype plugin indent on

let g:markdown_fenced_languages = ['apache', 'awk', 'bash=sh', 'basic', 'c', 'cheetah', 'cpp', 'cmake', 'crontab', 'css', 'cuda', 'dosbatch', 'go', 'html', 'java', 'json', 'linux-config=config', 'mail', 'mailcap', 'make', 'matlab', 'muttrc', 'mysql', 'objc', 'perl', 'perl6', 'php', 'pov', 'python', 'r', 'ruby', 'sql', 'svg', 'tex', 'tmux', 'vb', 'vcard', 'vim', 'xml', 'yaml']
let g:markdown_fold_style = 'nested'

au BufNewFile,BufRead *.md setl textwidth=0 autoindent nocindent fileformat=unix formatoptions+=ct

" Python {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

au BufNewFile,BufRead *.py setl tabstop=4 softtabstop=4 shiftwidth=4 textwidth=0 colorcolumn=80 expandtab autoindent fileformat=unix foldmethod=marker
" TODO How to enable foldmethod=indent in Python?

" R {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Coloring R output
let g:rout_follow_colorscheme = 1
let g:Rout_more_colors = 1
let g:R_assign = 0 " Disable replacement of "_" by " <- ".

if has("autocmd")
  if v:version > 701
	au BufRead,BufNewFile *.R setlocal foldmethod=marker tabstop=4 expandtab colorcolumn=80 textwidth=0

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

" Display syntax highlighting group in status bar
"set statusline+=%{SyntaxItem()}

" Error formats {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"set errorformat& " Reset to default value
set errorformat=%-GDEBUG%.%#                           " Ignore all debug messages, starting by DEBUG.
set errorformat+=%-G[%.%#INFO%.%#]%.%#                  " Maven info message. We ignore all of them.
set errorformat+=%A\ %#[javac]\ %f:%l:\ %m,%-Z\ %#[javac]\ %p^,%-C%.%# " Ant/Java. Put at beginning of list, otherwise another takes precedence.

set errorformat+=Error\ in\ %f\ (line\ %l)      " Matlab    Note that it requires that the Matlab output be filtered first in order to add the .m extension to the file.

" R
"set errorformat^=%.%#[exec]%.%#\ %f:%l:%c:\ %m    " R error when run with devtools package
"set errorformat+=%.%#(from\ %f#%l)%m  " R error when run with devtools package

" R testthat
" ── 1. Error: annotateMzValues() works correctly. (@BiodbCompounddbConn.R#121)  ─
set errorformat+=%.%#.\ Error:\ %m\ (@%f#%l)\ %.%#

set errorformat+=%m\ at\ %f:%l        " R testthat error
set errorformat+=%m\ in\ %f\ (line\ %l\\,\ column\ %c) " BiocCheck error
set errorformat+=%m\ (%f\\,\ line\ %l):\ %.%#  " BiocCheck error
set errorformat+=%f\ (line\ %l\\,\ column\ %c) " BiocCheck error
set errorformat+=%f\ (line\ %l) " BiocCheck error
set errorformat+=%f:%l\ %m " BiocCheck error

set errorformat+=%m\ at\ %f\ line\ %l.          " Perl

set errorformat+=%f:%l:\ %m " Custom file read with cfile

" Java
set errorformat+=[%.%#WARNING%.%#]\ %f:[%l\\,%c]\ %m    " Maven warning message. `%.%#` is here to catch color codes.
set errorformat+=[%.%#ERROR%.%#]\ %f:[%l\\,%c]\ %m      " Maven error message. `%.%#` is here to catch color codes.
"set errorformat-=%f\\|%l\\|\ %m
"set errorformat+=%.%#\ %f\\|%l\\|\ %m        " ant/Java
"set errorformat+=%f\\|%l\\|\ %m

" ant/Java
"let fmt = '%A\ %#[javac]\ %f:%l:\ %m,'
"	\ . '%A\ %#[aapt]\ %f:%l:\ %m,'
"	\ . '%-Z\ %#[javac]\ %p^,'
"	\ . '%C\ %#[javac]\ %m,'
"	\ . '%-C%.%#'
"execute 'set errorformat+=' . fmt

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

" Key bindings {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Correct ambiguous abbreviation on schroeder installation of vim (ambiguity between ELP and Explore).
cabbrev E Explore 

" Key bindings
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
nmap se :SpellCheck en<CR>
nmap sf :SpellCheck fr<CR>
nmap si :SpellCheck it<CR>
nmap sn :se nospell<CR>
"    S  delete current line and go into insert mode.
nmap t  gt
nmap T  gT
"    x  cut
"    zb Put cursor line at bottom
"    zc Close a fold.
nmap zh :noh<CR>
nmap zj :%!python -m json.tool<CR>
"    zm Close all folds by one level.
"    zo Open a fold.
"    zr Open all folds by one level.
nmap zs :source $MYVIMRC<CR>
"    zt Put cursor line at top
nmap zT :TabooRename 
"    zz center cursor line

" TODO create a key binding for editing file under cursor and jumping to first occurence of a specified word.
" Format: `some/path/to/a/file.txt#chapter1`.
" Use isfname to know which character are allowed in a filename.


