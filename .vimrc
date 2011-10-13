if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
set guioptions-=T
set nocompatible
map! <S-Insert> <MiddleMouse>
nmap gx <Plug>NetrwBrowseX
" autocmd VimEnter * NERDTree

nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
map <S-Insert> <MiddleMouse>
let &cpo=s:cpo_save
unlet s:cpo_save
colors darkspectrum 
" colors baycomb 
" colors asmanian_blood
syntax on

"fuzzysearch mappings
map <leader>f :FufFile **/<CR>
map <leader>b :FufBuffer<CR>

" Bubble single lines
nmap <C-k> ddkP
nmap <C-j> ddp

" Bubble multiple lines
vmap <C-k> xkP`[V`]
vmap <C-j> xp`[V`]

" Visually select the text that was last edited/pasted
nmap gV `[v`]

" set background=dark
set backspace=indent,eol,start
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set guifont=Terminus\ 8
set helplang=en
set hidden
set history=500
set hlsearch
set autoindent
set nomodeline
set mouse=a
set printoptions=paper:letter
set ruler
set shiftwidth=2
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set tabstop=2
set termencoding=utf-8
set title
set wildmenu
filetype on
filetype plugin on
autocmd BufReadPre,FileReadPre	*.tpl set nowrap

" vim: set ft=vim :

" cleaning up macros

" filename
let @z = expand("%:t:r")

"insert filename to the beginning of the file
let @l='ggO"zpA0jmj:w'
let @u=':wgg$vb"xy:€kunvf"xvf"h"cxxiclass=""xpa"ggo."xpa{"cpa}'
