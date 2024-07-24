function! s:Q() abort
	if &formatprg !=# ''
		let view = winsaveview()
		execute 'silent %!' .. &formatprg
		if v:shell_error
			echoerr 'format error'
			return
		endif
		call winrestview(view)
	endif
	write
endfunction

nnoremap Q <CMD>call <SID>Q()<CR>
