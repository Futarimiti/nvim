" setlocal shiftwidth=2
" setlocal tabstop=2
" setlocal expandtab

let b:start = '-wait=always node %'
lua vim.treesitter.start()

setlocal formatprg=js-beautify\ --editorconfig
