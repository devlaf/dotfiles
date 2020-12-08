"--------------------------------
" Devin LaFrance -- .vimrc
" -------------------------------

" Install vim-plug
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Plugins
call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-commentary'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'flazz/vim-colorschemes'
call plug#end()

" Settings - syntax highlighting
filetype plugin indent on
syntax on

" Settings - indentation
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set smarttab
set shiftround
set autoindent
set smartindent

" Settings - search
set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch
set gdefault

" Settings - column width
set textwidth=120
set colorcolumn=+1

" Settings - disable backup
set nobackup
set nowritebackup
set noswapfile

" Settings - misc
set hidden
set autoread
set virtualedit+=block

" Bindings
let mapleader = ","
nnoremap <C-h> <C-w>h    " split nav
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap / /\v           " search regex
vnoremap / /\v
noremap j gj             " navigate with word wrap
noremap k gk

" Theme
colorscheme koehler

" Remove trailing whitespace on save
autocmd BufWritePre * :%s/\s\+$//e

