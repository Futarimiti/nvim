" view diff of a modified file vs original state on the disk
function Diffthis() abort
  if !&l:modified
    echo 'No change'
    return
  endif

  vertical new
  setlocal buftype=nofile
  read #
  0delete _
  diffthis
  wincmd p
  diffthis
endfunction

command Diffthis call Diffthis()
