vim.opt_local.includeexpr = [[substitute(v:fname,'\.','/','g')]]
vim.opt_local.suffixesadd = { '.hs', '.lhs' }
-- FIXME: `import "package-name"Path.To.Module` gives `"Path.To.Module`
-- however should suffice most cases with proper spacing
-- Maybe check out src/search.c:3495 for reasons
vim.opt_local.include = [[\v^import(.*"|\s+(safe\s+)?(qualified)?)]]
vim.opt_local.define = [[^\(data\s*\|type\s*\|newtype\s*\|\s*\ze\i\+\s*\(::\|.*\s=\|<-\)\)]]
