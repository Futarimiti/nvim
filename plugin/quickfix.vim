nnoremap [q <CMD>silent cprevious<CR>
nnoremap ]q <CMD>silent cnext<CR>
nnoremap [c <CMD>silent cprevious<CR>
nnoremap ]c <CMD>silent cnext<CR>
nnoremap [l <CMD>silent lprevious<CR>
nnoremap ]l <CMD>silent lnext<CR>

function! s:toggle_loclist() abort
	if win_getid()->getwininfo()[0].loclist
		lclose
	else
		silent! lopen
	endif
endfunction

function! s:toggle_quickfix() abort
	if win_getid()->getwininfo()[0].quickfix
		cclose
	else
		copen
	endif
endfunction

nnoremap <localleader>q <CMD>call <SID>toggle_quickfix()<CR>
nnoremap <localleader>c <CMD>call <SID>toggle_quickfix()<CR>
nnoremap <localleader>l <CMD>call <SID>toggle_loclist()<CR>
