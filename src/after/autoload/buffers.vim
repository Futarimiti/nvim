" bdelete all buffers not being displayed in any window
function buffers#purge(force) abort
  let bang = a:force ? '!' : ''
  call getbufinfo()
        \->filter({ _, b -> b.windows->empty() })
        \->map({ _, b -> execute($'silent! bdelete{bang} {b.bufnr}') })
endfunction
