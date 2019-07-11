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
nnoremap <leader>f :bn<cr>
nnoremap <leader>d :bp<cr>
nnoremap <leader>ba :BufExplorer<cr>
imap jj <Esc>

filetype off
filetype indent plugin on

" -------------------------
"  fonts and colors
" -------------------------

set guifont=Monospace\ 10
if has("gui_running")
    colorscheme darkblue
else
    colorscheme murphy
endif

" -------------------------
"  filetypes
" -------------------------

au BufNewFile,BufRead *.rs set filetype=rust
au BufNewFile,BufRead *.rktl set filetype=scheme
au BufNewFile,BufRead *.go set filetype=go
au BufNewFile,BufRead *.ast set filetype=java
au BufNewFile,BufRead *.jrag set filetype=java
au BufNewFile,BufRead *.jadd set filetype=java

" -------------------------
"  plugins
" -------------------------

call plug#begin("~/.vim/plugged")
Plug 'rust-lang/rust.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()

" -------------------------
"  clang-format / rustfmt
" -------------------------

function ClangFormatFile()
    let l:lines="all"
    if filereadable("/usr/share/clang/clang-format.py")
        pyf /usr/share/clang/clang-format.py
    elseif filereadable("/usr/share/vim/addons/syntax/clang-format-3.7.py")
        pyf /usr/share/vim/addons/syntax/clang-format-3.7.py
    endif
endfunction
au BufNewFile,BufRead *.cc map <C-i> :call ClangFormatFile()<cr>
au BufNewFile,BufRead *.cpp map <C-i> :call ClangFormatFile()<cr>
au BufNewFile,BufRead *.c map <C-i> :call ClangFormatFile()<cr>
au BufNewFile,BufRead *.C map <C-i> :call ClangFormatFile()<cr>
au BufNewFile,BufRead *.h map <C-i> :call ClangFormatFile()<cr>
au BufNewFile,BufRead *.hpp map <C-i> :call ClangFormatFile()<cr>

function RustFormatFile()
    %!rustfmt
endfunction
au BufNewFile,BufRead *.rs map <C-i> :call RustFormatFile()<cr>
