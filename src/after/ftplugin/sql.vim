setlocal formatprg=sqlformat\ -
" setlocal comments=:--
" setlocal commentstring=--\ %s
setlocal include=\\csource
" PostgreSQL provides doc for some commands
" though not to expect 100% coverage
" setlocal keywordprg=:Man\ 7

" vim-dadbod-completion
packadd! vim-dadbod-completion
setlocal omnifunc=vim_dadbod_completion#omni

lua vim.treesitter.start()
