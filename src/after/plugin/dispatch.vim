packadd vim-dispatch

nnoremap `<CR> <Cmd>Dispatch!<CR>
nnoremap m<CR> <Cmd>Make!<CR>

" space after bang
nnoremap m! :Make!<Space>
nnoremap `! :Dispatch!<Space>
nnoremap '! :Start!<Space>
nnoremap g'! :Spawn!<Space>

" wait
nnoremap '- :Start -wait=always<Space>
nnoremap g'- :Spawn -wait=always<Space>

" ...
nmap · `
nmap ‘ '
