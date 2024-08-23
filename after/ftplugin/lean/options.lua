vim.opt.wildignore:append [[*.olean]]
vim.opt.suffixes:append [[.olean]]

vim.bo.iskeyword = [[a-z,A-Z,_,48-57,192-255,!,',?]]
vim.bo.comments = [[s0:/-,mb:\ ,ex:-/,:--]]
vim.bo.commentstring = [[/- %s -/]]

vim.bo.includeexpr = [[substitute(v:fname, '\.', '/', 'g')]]
vim.opt_local.suffixesadd = { '.lean' }

vim.bo.expandtab = true
vim.bo.shiftwidth = 2
vim.bo.softtabstop = 2

vim.opt_local.matchpairs:append { [[⟨:⟩]], [[‹:›]] }

-- Matchit support
if vim.g.loaded_matchit and not vim.b.match_words then
  vim.b.match_ignorecase = 0
  vim.b.match_words = table.concat({
    [[\<\%(namespace\|section\)\s\+\(.\{-}\)\>:\<end\s\+\1\>]],
    [[^\s*section\s*$:^end\s*$]],
  }, ',')
end
