setlocal spell
setlocal wrap
setlocal keywordprg=texdoc\ --nointeract
let &l:formatprg = 'latexindent -g /dev/null -'

" for autocomplete only
" cannot directly set isk, otherwise \cmd\cmd would be recognised as one word
let b:autocomplete_extra_isk = '\'

" $VIMRUNTIME/ftplugin/tex.vim handles most include options
setlocal suffixesadd+=.latex

compiler latexmk

nnoremap <buffer> <leader>b <CMD>make -interaction=nonstopmode -p %:S<CR>
nnoremap <buffer> <leader>e
      \ <CMD>make -interaction=nonstopmode -synctex=1 -pv %:S<CR>
