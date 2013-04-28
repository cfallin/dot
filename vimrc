set nocompatible

" --------------------------
"  basic options
" --------------------------
set ts=4
set sw=4
set et
set bs=2
set incsearch
set smartindent
set modelines=1
set number
set hidden
set guioptions-=m " no menubar
set guioptions-=T " no tab bar
syntax on
set ruler
set encoding=utf-8
"set scrolloff=3
set autoindent
set showmode
set showcmd
set wildmenu
set wildmode=list:longest
"set undofile
let mapleader = ","
set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch
nnoremap <leader><space> :noh<cr>
nnoremap <leader>q gqip

filetype off
filetype indent plugin on

" -------------------------
"  fonts and colors
" -------------------------

set guifont=Monospace\ 11
if has("gui_running")
    colorscheme darkblue
endif


call pathogen#infect()
call pathogen#helptags()

"let g:miniBufExplorerMoreThanOne=1
let g:miniBufExplMapWindowNavVim=1

nnoremap <leader>f :bn<cr>
nnoremap <leader>d :bp<cr>

au BufNewFile,BufRead *.rs set filetype=rust
au BufNewFile,BufRead *.rktl set filetype=scheme
au BufNewFile,BufRead *.go set filetype=go
