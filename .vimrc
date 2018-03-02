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
Plug 'lepture/vim-jinja'
Plug 'chase/vim-ansible-yaml'
call plug#end()

colorscheme molokai
let g:rehash256 = 1

let g:Powerline_symbols = 'fancy'
let g:Gtags_OpenQuickfixWindow = 0
let g:go_version_warning = 0

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
