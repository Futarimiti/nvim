" adapted from https://github.com/neovimhaskell/haskell-vim/blob/master/indent/cabal.vim
function cabal#get_indent() abort
  let l:prevline = getline(v:lnum - 1)

  if l:prevline =~# '^\(executable\|library\|flag\|source-repository\|test-suite\|benchmark\)'
    return 2
  else
    return match(l:prevline, '\S')
  endif
endfunction

