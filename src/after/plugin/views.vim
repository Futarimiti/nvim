set viewoptions-=curdir

" :View gets current view file (if exists)
" :View! to delete view file of current buffer
command -bang View call s:view(<bang>0)

" rm -rf <viewdir>
command -bang ClearViews
      \ if isdirectory(&viewdir) | call delete(&viewdir, 'rf') | endif

function s:view(bang) abort
  let viewfile = bufname()->s:viewfile()
  if viewfile is 0
    echohl ErrorMsg
    echo 'No view'
    echohl Normal
    return
  endif

  if a:bang
    call delete(viewfile)
  else
    echo viewfile
  endif
endfunction

" work out filepath to viewfile of given file, if exists
" else return 0
" example:
" '~/.config/nix-darwin/neovim/after/plugin/views.vim'
" -> '~=+.config=+nix-darwin=+neovim=+after=+plugin=+views.vim='
function s:viewfile(file) abort
  let basename = a:file
        \->fnamemodify(':~')
        \->substitute('/', '=+', 'g')
        \.. '='
  let filename = &viewdir->fnamemodify(':p') .. basename
  if filereadable(filename)
    return filename->simplify()
  endif
endfunction
