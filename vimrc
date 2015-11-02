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
else
    colorscheme murphy
endif


call pathogen#infect()
call pathogen#helptags()

"let g:miniBufExplorerMoreThanOne=1
let g:miniBufExplMapWindowNavVim=1

let g:ycm_path_to_python_interpreter = '/usr/bin/python2'

nnoremap <leader>f :bn<cr>
nnoremap <leader>d :bp<cr>

nnoremap <leader>ba :BufExplorer<cr>

au BufNewFile,BufRead *.rs set filetype=rust
au BufNewFile,BufRead *.rktl set filetype=scheme
au BufNewFile,BufRead *.go set filetype=go

imap jj <Esc>

function ClangFormatFile()
    let l:lines="all"
    if filereadable("/usr/share/clang/clang-format.py")
        pyf /usr/share/clang/clang-format.py
    elseif filereadable("/usr/share/vim/addons/syntax/clang-format-3.7.py")
        pyf /usr/share/vim/addons/syntax/clang-format-3.7.py
    endif
endfunction
map <C-i> :call ClangFormatFile()<cr>
