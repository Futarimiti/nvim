-- NOTE: setting includeexpr in this naive way causes include-search
-- frequently result in a directory rather than the hs source file,
-- making the search result incomplete;
-- however a full result could break vim for searching too many files,
-- maybe just remain status quo?
vim.opt_local.includeexpr = [[substitute(v:fname,'\.','/','g')]]
vim.opt_local.suffixesadd = { '.hs', '.lhs' }

-- FIXME: `import "package-name"Path.To.Module` gives `"Path.To.Module`
-- however should suffice most cases with proper spacing
-- Maybe check out src/search.c:3495 for reasons
vim.opt_local.include = [[\v^import(.*"|\s+(safe\s+)?(qualified)?)]]
vim.opt_local.define = [[^\(data\s*\|type\s*\|newtype\s*\|\s*\ze\i\+\s*\(::\|.*\s=\|<-\)\)]]

-- haskdogs
local haskdogs = vim.fn.glob '~/.haskdogs/'
if vim.fn.executable 'haskdogs' == 1 and haskdogs ~= '' then
  -- most packages put library in ., some in src/, some in lib/
  -- there are exceptions, but these should handle 90%+
  vim.opt_local.path:append { haskdogs .. '*', haskdogs .. '*/src', haskdogs .. '*/lib' }
end
