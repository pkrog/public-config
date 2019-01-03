" vi: fdm=marker

" Indentation {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set noexpandtab
set copyindent
set preserveindent
set softtabstop=0
set shiftwidth=4
set tabstop=4
set cindent
set cinoptions=(0,u0,U0)

" Plugins loaded with pathogen {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Must go first, otherwise markdown-folding plugin won't work.
if has("autocmd")
	filetype plugin indent on
endif

" Enable loading of plugins stored inside ~/.vim/bundle folder.
execute pathogen#infect()

" Do not run help tags generation, because it creates tag files in each bundle directory `bundle/<module>/doc/ctags`
" execute pathogen#helptags()

" Markdown {{{2

let g:markdown_fenced_languages = ['apache', 'awk', 'bash=sh', 'basic', 'c', 'cheetah', 'cpp', 'cmake', 'crontab', 'css', 'cuda', 'dosbatch', 'go', 'html', 'java', 'json', 'linux-config=config', 'mail', 'mailcap', 'make', 'matlab', 'muttrc', 'mysql', 'objc', 'perl', 'perl6', 'php', 'pov', 'python', 'r', 'ruby', 'sql', 'svg', 'tmux', 'vb', 'vcard', 'vim', 'xml', 'yaml']
let g:markdown_fold_style = 'nested'

" Syntax highlighting {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

syntax on
" TODO put the following inside dedicated file
au BufRead,BufNewFile *.prd set filetype=parse_recdescent
colorscheme darkscheme

" Included syntax highlighting {{{2

" Inside XML
autocmd Syntax xml call SyntaxRange#Include('^.*@@@BEGIN_RST@@@.*$', '^.*@@@END_RST@@@.*$', 'rst', 'NonText') " reStructuredText
autocmd Syntax xml call SyntaxRange#Include('^.*@@@BEGIN_CHEETAH@@@.*$', '^.*@@@END_CHEETAH@@@.*$', 'cheetah', 'NonText')
autocmd Syntax sh call SyntaxRange#Include('^.*@@@BEGIN_SQL@@@.*$', '^.*@@@END_SQL@@@.*$', 'sql', 'NonText')
autocmd Syntax sh call SyntaxRange#Include('^.*@@@BEGIN_PYTHON@@@.*$', '^.*@@@END_PYTHON@@@.*$', 'python', 'NonText')
" Commented out Markdown syntax highlighting inside XML, because it clashes with reStructuredText:
"     Error detected while processing /usr/local/Cellar/vim/8.0.1200/share/vim/vim80/syntax/vim.vim:
"     line  791:
"     E403: syntax sync: line continuations pattern specified twice
"autocmd Syntax xml call SyntaxRange#Include('^.*@@@BEGIN_MARKDOWN@@@.*$', '^.*@@@END_MARKDOWN@@@.*$', 'markdown', 'NonText')

" TODO & co highlighting {{{2

" FIXME When splitting a window, the highlighting disappears.
if has("autocmd")
  " Highlight TODO, FIXME, XXX, NOTE, INFO, IDEA, URGENT, DEPRECATED etc everywhere (not only in comments)
  if v:version > 701
    autocmd Syntax * call matchadd('Todo',  '\W\zs\(TODO\|FIXME\|XXX\)')
    autocmd Syntax * call matchadd('Debug', '\W\zs\(NOTE\|INFO\|IDEA\)')
    autocmd Syntax * call matchadd('Error', '\W\zs\(URGENT\|DEPRECATED\|BUG\|ERROR\|IMPORTANT\)')
  endif
endif

" Search & replace {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set hlsearch

" Make {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
 
autocmd syntax make set list
autocmd syntax make set listchars=tab:\ \ 
set autowrite

" FILE AUTOCOMPLETION {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set wildmenu
set wildmode=longest:full,full
set wildignore+=*.a,*.o
set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.png
set wildignore+=.DS_Store,.git,.hg,.svn
set wildignore+=*~,*.swp,*.tmp

" ENCODING AND FILE FORMAT {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set enc=utf-8
set fileformat=unix

" MISC {{{1
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
set statusline=%n:%<%f%m%=%l,%c-%P%Y%R

function! SyntaxItem()
"	  return synIDattr(synID(line("."),col("."),1),"name")
  return join(map(synstack(line("."), col(".")), 'synIDattr(v:val, "name")'))
endfunction

" Display syntax highlighting group in status bar
"set statusline+=%{SyntaxItem()}

" AUTO-COMMENT {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Disable auto-comment for all file types
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Error formats {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set errorformat& " Reset to default value
set errorformat^=%A\ %#[javac]\ %f:%l:\ %m,%-Z\ %#[javac]\ %p^,%-C%.%# " Ant/Java. Put at beginning of list, otherwise another takes precedence.
"set errorformat^=%.%#[exec]%.%#\ %f:%l:%c:\ %m    " R error when run with devtools package
"set errorformat+=%.%#(from\ %f#%l)%m  " R error when run with devtools package
set errorformat+=%m\ at\ %f:%l        " R testthat error
set errorformat+=Error\ in\ %f\ (line\ %l)      " Matlab    Note that it requires that the Matlab output be filtered first in order to add the .m extension to the file.
set errorformat+=%m\ at\ %f\ line\ %l.          " Perl
set errorformat+=[WARNING]\ %f:[%l\\,%c]\ %m    " Maven
set errorformat+=[ERROR]\ %f:[%l\\,%c]\ %m      " Maven
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

" SPELL CHECKING {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

for lang in ['fr', 'en', 'it']
	let wordsfile = expand("~/.vim/spell/" . lang . ".utf-8.add")
	let splfile = wordsfile . ".spl"
	if (! filereadable(splfile)) || getftime(splfile) < getftime(wordsfile)
		exec "silent mkspell! " . wordsfile 
	endif
endfor
command! -nargs=1 SpellCheck setlocal spelllang=<args> | set spell

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

" Paths {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set path+=~/dev/public-notes
set path+=~/dev/private-notes

" Swap files {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set directory^=~/tmp/vim.swp//
set backupdir^=~/tmp/vim.bkp//
