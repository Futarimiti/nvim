setlocal spell
setlocal wrap
setlocal keywordprg=texdoc\ --nointeract
let &l:formatprg = 'latexindent -g /dev/null -'

" for autocomplete only
" cannot directly set isk, otherwise \cmd\cmd would be recognised as one word
let b:autocomplete_extra_isk = '\'

" include options set by $VIMRUNTIME/ftplugin/plaintex.vim
