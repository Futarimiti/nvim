setlocal formatprg=sqlformat\ -
setlocal commentstring=--\ %s
setlocal include=\\csource
" PostgreSQL provides doc for some commands
" though not to expect 100% coverage
setlocal keywordprg=:Man\ 7
