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
set maxmempattern=2000

if has('win32') || has('win64')
    set runtimepath-=~/vimfiles
    set runtimepath^=~/.vim
    set runtimepath-=~/vimfiles/after
    set runtimepath+=~/.vim/after
endif

if has("gui_win32")
    set guifont=Monaco:h11:cANSI
    set guioptions=
    set clipboard=unnamed
endif

set nocompatible
call plug#begin('~/.vim/plugged')
Plug 'VundleVim/Vundle.vim'
Plug 'Lokaltog/vim-powerline'
Plug 'xolox/vim-misc'
Plug 'dimasg/vim-mark'
Plug 'easymotion/vim-easymotion'
Plug 'tpope/vim-fugitive'
Plug 'rdolgushin/groovy.vim'
Plug 'airblade/vim-gitgutter'
Plug 'saltstack/salt-vim'
Plug 'fatih/vim-go'
Plug 'rust-lang/rust.vim'
Plug 'lepture/vim-jinja'
Plug 'chase/vim-ansible-yaml'
Plug 'zhou13/vim-easyescape'
Plug 'altercation/vim-colors-solarized'
Plug 'tpope/vim-surround'
call plug#end()

if has("gui_running")
    set background=light
    colorscheme solarized
else
    colorscheme molokai
endif

let g:rehash256 = 1

let g:Powerline_symbols = 'fancy'
let g:Gtags_OpenQuickfixWindow = 0
let g:go_version_warning = 0
let g:go_gopls_enabled = 0

au BufRead,BufNewFile *.yang set filetype=yang
au BufRead,BufNewFile *.groovy set filetype=groovy
au BufRead,BufNewFile Jenkinsfile set filetype=groovy
au BufRead,BufNewFile .spacemacs set filetype=lisp
autocmd BufWritePre * %s/\s\+$//e

inoremap <F1> <nop>
map  <Space> <Plug>(easymotion-bd-f)
nmap <Space> <Plug>(easymotion-overwin-f)
if &diff
    nmap <C-j> <Plug>GitGutterNextHunk
    nmap <C-k> <Plug>GitGutterPrevHunk
endif

let g:easyescape_chars = { "j": 1, "k": 1 }
if has('python3')
    let g:easyescape_timeout = 100
endif
cnoremap jk <ESC>
cnoremap kj <ESC>

xmap s <Plug>VSurround
nmap + <C-a>
nmap - <C-x>

if &diff
    colorscheme evening
endif
