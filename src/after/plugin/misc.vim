nnoremap <C-C> <ESC>
inoremap <C-C> <ESC>
xnoremap <C-C> <ESC>
inoremap <C-Z> <C-O>zz

set nowrap
set path+=**
set fillchars+=eob:\ 
set autowrite
set colorcolumn=+1
set signcolumn=yes:1
set noshowmode
set shortmess+=aoOstTWAIcCqF
set guifont=JetBrainsMono\ NF:h13
set smoothscroll

" g<Tab>l would be easier to type than ]l
" can't use noremap - loses ] remaps
map g<Tab> ]
map g<S-Tab> [

" remove blank lines
command -range=% NoBlank <line1>,<line2>global/^\s*$/delete

" capitalise
nnoremap <silent> gC :s/\v<(.)(\w*)/\u\1\L\2/g<Bar>noh<CR>
xnoremap <silent> gC :s/\v<(.)(\w*)/\u\1\L\2/g<Bar>noh<CR>
