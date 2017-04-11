syntax on
" set nu!
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set hlsearch
set incsearch
set wildmenu
set ruler
set rulerformat=%20(%2*%<%f%=\ %m%r\ %3l\ %c\ %p%%%)
set cc=120
set backspace=indent,eol,start
set fileencodings=utf-8,gbk,ucs-bom,cp936

set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'Lokaltog/vim-powerline'
Plugin 'xolox/vim-misc'
Plugin 'dimasg/vim-mark'
Plugin 'easymotion/vim-easymotion'
Plugin 'tpope/vim-fugitive'
call vundle#end()
filetype plugin indent on
filetype plugin on

let g:Powerline_symbols = 'fancy'
let g:Gtags_OpenQuickfixWindow = 0

au BufRead,BufNewFile *.yang set filetype=yang
autocmd BufWritePre * %s/\s\+$//e

inoremap <F1> <nop>
map  <Space> <Plug>(easymotion-bd-f)
nmap <Space> <Plug>(easymotion-overwin-f)
