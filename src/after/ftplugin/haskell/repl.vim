let b:ferrum = 'ghci %'
let b:ferrum_process = {lines->lines->insert(':{')->add(':}')}

augroup GHCiAutoReload
  autocmd!
  autocmd BufWritePost <buffer> silent! SendlnREPL :r
augroup END

xnoremap <buffer> <silent> <Leader>r 
        \:<C-U>SendlnREPL :{<CR>
        \:'<,'>SendRangeREPL<CR>
        \:SendlnREPL :}<CR>
