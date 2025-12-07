setlocal spell
setlocal wrap
setlocal keywordprg=texdoc\ --nointeract
let &l:formatprg = 'latexindent -g /dev/null -'
" include options mostly set by $VIMRUNTIME/ftplugin/tex.vim
setlocal suffixesadd+=.latex
