" also compatible for tabline
function statusline#escape(str)
  return a:str->substitute('%', '%%', 'g')
endfunction
