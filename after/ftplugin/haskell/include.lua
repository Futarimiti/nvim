vim.opt_local.includeexpr = [[substitute(v:fname,'\.','/','g')]]
vim.opt_local.suffixesadd = { '.hs', '.lhs' }
vim.opt_local.include =
  [[\v^\s*import\s*(safe\s*)?(qualified\s*)?("\zs[^"]+\ze")?\s*\zs[0-9A-Za-z\._]+]]
vim.opt_local.define = [[^\(data\s*\|type\s*\|newtype\s*\|\s*\ze\i\+\s*\(::\|.*\s=\|<-\)\)]]
