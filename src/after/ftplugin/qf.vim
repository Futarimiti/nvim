let &l:statusline = "%{w:->get('quickfix_title', '')}%< %= %t"

" packadd quicker.nvim

" quicker.nvim uses em-space as a marker for extmarks
" this could be however inconvenient (and potentially undermining)
" when you yank a line with em-space thinking it's just normal space.
" this autocmd substitutes all em-spaces to normal spaces as you yank;
" a much less-XXX approach would be to just stop using em-spaces
augroup SubstituteEmspaces
	autocmd!
	autocmd TextYankPost <buffer> call s:textyankpost()
augroup END

function s:textyankpost() abort
	let EM_SPACE = "\u2003"
	let processed = v:event['regcontents']
				\->mapnew({ _, line -> line->substitute(EM_SPACE, ' ', 'g') })
	let changed = v:event['regcontents'] !=# processed
	if changed
		echohl WarningMsg
		echomsg 'qf: detected U+2003 em space, substituting'
		echohl None
		call setreg(v:event['regname'], processed)
	endif
endfunction

" example
" before: neovim/after/plugin/browsing/autojump.vim augroup dirfootprint
" after:  neovim/after/plugin/browsing/autojump.vim augroup dirfootprint
"                                                  ^

" em-space encoded as 0xE2 0x80 0x83 in utf8
" 0xE2 = 226
set isfname+=^226

