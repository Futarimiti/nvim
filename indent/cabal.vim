" adapted from https://github.com/neovimhaskell/haskell-vim/blob/master/indent/cabal.vim
if exists('b:did_indent')
  finish
endif
let b:did_indent = 1

function! s:GetCabalIndent()
  let l:prevline = getline(v:lnum - 1)

  if l:prevline =~# '^\(executable\|library\|flag\|source-repository\|test-suite\|benchmark\)'
    return 2
  else
    return match(l:prevline, '\S')
  endif
endfunction

let &l:indentexpr = expand('<SID>') .. 'GetCabalIndent()'
setlocal indentkeys=!^F,o,O,<CR>
