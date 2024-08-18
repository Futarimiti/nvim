setlocal spell
setlocal wrap
setlocal keywordprg=texdoc\ --nointeract
let &l:formatprg = 'latexindent -g /dev/null -'
" include options already set by $VIMRUNTIME/ftplugin/plaintex.vim
