" enable cursorline upon entering a window until leaving the window

augroup auto-cursorline
  autocmd!
  autocmd WinEnter * if getwinvar(0, 'auto_cursorline', v:false) | setlocal cursorline | unlet w:auto_cursorline | endif
  autocmd WinLeave * if &cursorline | let w:auto_cursorline = 1 | setlocal nocursorline | endif
augroup END
