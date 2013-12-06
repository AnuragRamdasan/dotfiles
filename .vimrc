"=======================================================
"BASIC VIM SETTINGS
"=======================================================
set runtimepath=/usr/share/vim/vim74a
set nocompatible               " be iMproved
set smartindent
"highlight search results
set hlsearch
"make backspace be backspace
set backspace=indent,eol,start
"make searches act like in modern browsers
set incsearch
"show what is being typed as a command
set showcmd
set nu
"always show current position
set ruler
"set how many lines of history vim remembers
set history=700
"set autoread when file is changed from the outside
set autoread
"turn on the wildmenu
set wildmenu
"Ignore all compiled files
set wildignore=*.o,*~,*.pyc
"set height of command bar
set cmdheight=2
"show matching brackets when text indicator is over them
set showmatch
"highlight current line
set cul
"enable syntax highlighting
syntax on
"set utf8 as standard encoding
set encoding=utf8
"helps make file loading faster
set lazyredraw
"faster connection over terminal
set ttyfast
"remove toolbar
set guioptions-=T
"remove right and left scrollbar 
set guioptions-=r
set guioptions-=L
"set tab settings
set expandtab		"convert tabs to spaces
set smarttab

set shiftwidth=8
set tabstop=4

set wrap

"turn backups and swap files off
set nobackup
set nowb
set noswapfile

"=====================================================
"REMAPPING
"=====================================================
" map tab switching
map <F9> :tabprevious<CR>
map <S-F9> :tabnext<CR>


"=====================================================
" ALL ADDED COLORSCHEMES
"=====================================================
colorscheme hemisu
set background=dark

filetype off                   " required!
filetype plugin indent on     " required!
filetype plugin on

"=====================================================
"ALL VUNDLE STUFF
"=====================================================
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'
   
" My Bundles here:
" nerdtree          : file explorer
" jedi-vim          : auto complete for python
" vim-nerdtree-tabs : permanent nerdtree on all tabs
" fugitive          : git integration
" syntastic         : error highlighter
" ctrlp             : fuzzy search
" vim-easymotion    : easy navigation
" closetag          : html xml auto close tags
" tagbar            : ctags based function lists
" supertab          : autocomplete
" detectindent      : autoindent
" numbers           : smart numbers for vim
Bundle 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
Bundle 'scrooloose/nerdtree'              
Bundle 'klen/python-mode'
Bundle 'davidhalter/jedi-vim'             
Bundle 'jistr/vim-nerdtree-tabs'          
Bundle 'tpope/vim-fugitive'              
Bundle 'scrooloose/syntastic'             
Bundle 'kien/ctrlp.vim'                   
Bundle 'Lokaltog/vim-easymotion'          
Bundle 'docunext/closetag.vim'
Bundle 'majutsushi/tagbar'
Bundle 'ervandew/supertab'
Bundle 'ciaranm/detectindent'
Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
Bundle 'Valloric/YouCompleteMe'
Bundle 'myusuf3/numbers.vim'
Bundle 'rosenfeld/conque-term'
Bundle 'sjl/gundo.vim'
"====================================================
"AUTOCOMMANDS
"====================================================
augroup vimrc_autocmds
	autocmd!
	
    autocmd FileType python highlight Excess ctermbg=DarkGrey guibg=Black
	autocmd FileType python match Excess /\%120v.*/
	autocmd FileType python set nowrap
    autocmd Filetype html,xml,xsl source ~/.vim/scripts/closetag.vim 
    autocmd FileType html setlocal shiftwidth=2 tabstop=2
    "code folding settings
    "use zo to open folds and zc to close
    au BufReadPre * setlocal foldmethod=indent
    au BufWinEnter * if &fdm == 'indent' | setlocal foldmethod=syntax | setlocal foldnestmax=1 | endif

augroup END

"======================================================
"CONFIG ADDED BUNDLES
"======================================================

"POWERLINE
"------------------------------------------------------
set laststatus=2

" NERDTREE
" -----------------------------------------------------
" map F2 to NerdTree toggle
au VimEnter *  NERDTree
map <F2> :NERDTreeToggle<CR>

" PYMODE
" -----------------------------------------------------
	let g:pymode_rope = 0 
	" Documentation
		let g:pymode_doc = 1
		let g:pymode_doc_key = 'K'
	"Linting
		let g:pymode_lint = 1
		let g:pymode_lint_checker = "pyflakes,pep8"
	" Auto check on save
		let g:pymode_lint_write = 1
	" Support virtualenv
		let g:pymode_virtualenv = 1
	" Enable breakpoints plugin
		let g:pymode_breakpoint = 1
		let g:pymode_breakpoint_key = '<leader>b'
	" syntax highlighting
		let g:pymode_syntax = 1
		let g:pymode_syntax_all = 1
		let g:pymode_syntax_indent_errors = g:pymode_syntax_all
		let g:pymode_syntax_space_errors = g:pymode_syntax_all
	" Don't autofold code
		let g:pymode_folding = 0

"CLOSETAG
"------------------------------------------------------
:let g:closetag_html_style=1
:source ~/.vim/scripts/closetag.vim

"TAGBAR
"------------------------------------------------------
map tb :TagbarToggle<CR>

"DETECTINDENT
"------------------------------------------------------
:let g:detectindent_preferred_expandtab=1
:let g:detectindent_preferred_indent=8

"SUPERTAB CONFIG
"------------------------------------------------------
:let g:SuperTabMappingForward = '<C-space>'
:let g:SuperTabMappingBackward = '<s-c-space>'

"Numbers setup
"------------------------------------------------------
nnoremap <F3> :NumbersToggle<CR>

" SYNTASTIC IGNORE SETTINGS
let g:syntastic_python_checker_args="--ignore=E501"
