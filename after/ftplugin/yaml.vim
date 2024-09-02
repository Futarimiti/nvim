if executable('yamlfmt')
  setlocal formatprg=yamlfmt\ -in
elseif executable('prettier')
  setlocal formatprg=prettier\ --parser\ yaml
endif
