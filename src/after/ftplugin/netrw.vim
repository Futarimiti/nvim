function s:cwd() abort
	return get(b:, 'netrw_curdir')
				\ ?? isdirectory(bufname())
				\ ? bufname()
				\ : getcwd()
endfunction

" edit existing or new file in cwd; better netrw-% alternative
nnoremap <buffer> <expr> % $':edit {<SID>cwd()}/'

" new directory (req eunuch)
packadd vim-eunuch
nnoremap <buffer> <expr> <nowait> d $':Mkdir {<SID>cwd()}/'

" trash (req vinegar, trash)
" sometimes does not work well with tree layout (as always)
packadd vim-vinegar
nmap <buffer> <nowait> D .!trash<End>
xmap <buffer> <nowait> D .!trash<End>

" rename/move (req vinegar)
nmap <buffer> <nowait> R .!mv<End> <C-R><C-F>

" prefer double click
silent! nunmap <buffer> <LeftMouse>
nmap <buffer> <2-LeftMouse> <CR>

" concealing
setlocal conceallevel=2
setlocal concealcursor=nvc
augroup NetrwConceal
	autocmd!
	" concealing gone upon pressing <CR>, must setup as autocmd
	autocmd TextChanged <buffer> syntax match NetrwTreePipe '|' conceal cchar=│
augroup END

" refresh
command -buffer -nargs=0 -bang -bar Refresh edit<bang> `=s:cwd()`
nnoremap <buffer> <C-L>
			\ <Cmd>nohlsearch<bar>diffupdate<bar>Refresh<bar>normal!<lt>C-L><CR>

" extending vinegar-. (req vinegar)
nmap <buffer> `. .Dispatch<Space>
xmap <buffer> `. .Dispatch<Space>
nmap <buffer> '. .Start<Space>
xmap <buffer> '. .Start<Space>
nmap <buffer> g'. .Spawn<Space>
xmap <buffer> g'. .Spawn<Space>
nmap <buffer> <Space>'. .Console<Space>
xmap <buffer> <Space>'. .Console<Space>
