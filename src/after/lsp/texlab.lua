---@type vim.lsp.Config
return {
  name = 'texlab',
  filetypes = { 'tex', 'plaintex', 'bib' },
  cmd = { 'texlab' },
  root_markers = {
    '.latexmkrc',
    '.texlabroot',
    'texlabroot',
    'Tectonic.toml',
  },
}
