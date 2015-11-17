syntax on
colorscheme peachpuff
set encoding=utf-8
set fileencodings=iso-2022-jp,euc-jp,sjis,utf-8
set fileformats=unix,dos,mac
set autoindent
set smartindent
set shiftwidth=4
set tabstop=4
set softtabstop=4
set backspace=indent,eol,start
set whichwrap=b,s,h,l,<,>,[,],~
set noswapfile
set nobackup
set wrap
set wrapscan
set wildmenu
set smartcase
set ignorecase
set hlsearch
set expandtab
set incsearch
set showcmd
set showmode
set notitle
set cursorline
highlight CursorLine cterm=underline ctermfg=NONE ctermbg=NONE
set showmatch
set number
set ruler
set laststatus=2
nnoremap ; :
nnoremap : ;
inoremap () ()<Left>
inoremap {} {}<Left>
inoremap [] []<Left>
inoremap <> <><Left>
inoremap "" ""<Left>
inoremap '' ''<Left>
inoremap `' `'<Left>
inoremap <C-A> <Home>
inoremap <C-E> <End>
inoremap <C-F> <Right>
inoremap <C-B> <Left>
inoremap <C-D> <Del>
inoremap <C-N> <Down>
inoremap <C-P> <Up>
imap <c-j> <esc>
set laststatus=2
