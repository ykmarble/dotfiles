syntax enable
set background=dark
colorscheme peachpuff
set encoding=utf-8
set fileencodings=utf-8,iso-2022-jp,euc-jp,sjis
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
set clipboard=unnamedplus
inoremap <C-a> <Home>
inoremap <C-e> <End>
inoremap <C-f> <Right>
inoremap <C-b> <Left>
inoremap <C-d> <Del>
inoremap <C-n> <Down>
inoremap <C-p> <Up>
nnoremap s <Nop>
nnoremap sj <C-w>j
nnoremap sk <C-w>k
nnoremap sl <C-w>l
nnoremap sh <C-w>h
nnoremap sJ <C-w>J
nnoremap sK <C-w>K
nnoremap sL <C-w>L
nnoremap sH <C-w>H
nnoremap tt :<C-u>tabnew<CR>
nnoremap tn gt
nnoremap tp gT
nnoremap ss :<C-u>sp<CR>
nnoremap sv :<C-u>vs<CR>
nnoremap sq :<C-u>q<CR>
nnoremap sQ :<C-u>bd<CR>
inoremap <C-j> <esc>
nnoremap <C-j> <esc>
vnoremap <C-j> <esc>
inoremap <C-c> <esc>
nnoremap <C-c> <esc>
vnoremap <C-c> <esc>

if !has('gui_running')
    augroup mytransp
        autocmd!
        autocmd VimEnter,ColorScheme * highlight Normal ctermbg=none
        autocmd VimEnter,ColorScheme * highlight LineNr ctermbg=none
        autocmd VimEnter,ColorScheme * highlight SignColumn ctermbg=none
        autocmd VimEnter,ColorScheme * highlight VertSplit ctermbg=none
        autocmd VimEnter,ColorScheme * highlight NonText ctermbg=none
    augroup END
endif

function! DisableIME()
    if executable("fcitx-remote")
        call system("fcitx-remote -c")
    endif
    if executable("fcitx5-remote")
        call system("fcitx5-remote -c")
    endif
    if executable("ibus")
        call system("ibus engine xkb:us::eng")
    endif
endfunction
autocmd InsertLeave * call DisableIME()
