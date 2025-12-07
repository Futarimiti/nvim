" setlocal shiftwidth=2
" setlocal tabstop=2
" setlocal expandtab

if &l:filetype isnot 'query'
	setlocal foldmethod=syntax
endif
