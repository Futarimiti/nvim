function! s:search_package_path(fname) abort
  return luaeval('package.path')
        \->substitute('?', a:fname, 'g')
        \->split(';')
        \->filter({_idx, path -> filereadable(path)})
        \->get(0)
endfunction

function! s:search_runtime(fname) abort
  let s:get_runtime = {pat -> nvim_get_runtime_file(pat, v:false)->get(0)}
  return s:get_runtime($'lua/{a:fname}.lua') ?? 
        \s:get_runtime($'lua/{a:fname}/init.lua') ??
        \s:get_runtime($'fnl/{a:fname}.fnl') ?? 
        \s:get_runtime($'fnl/{a:fname}/init.fnl')
endfunction

function! s:includeexpr(modname) abort
  let fname = a:modname->substitute('\.', '/', 'g')
  return s:search_package_path(fname) ??
        \s:search_runtime(fname)
endfunction

lua vim.opt_local.include = [=[\v<((do|load)file|require)\s*\(?['"]\zs[^'"]+\ze['"]]=]
let &l:includeexpr = expand('<SID>') .. 'includeexpr(v:fname)'
