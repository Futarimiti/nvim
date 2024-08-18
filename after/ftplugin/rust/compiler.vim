" compiler set in $VIMRUNTIME/ftplugin/rust.vim
if b:current_compiler ==# 'cargo'
  let b:start = '-wait=always cargo run'
  let b:dispatch = 'cargo build --color never'
elseif b:current_compiler ==# 'rustc'
  let b:dispatch = 'rustc --color never %'
  let b:start = executable('evcxr') ? 'evcxr' : ''
elseif executable('evcxr')
  let b:start = 'evcxr'
endif
