---@type vim.lsp.Config
return {
  name = 'jdtls',
  filetypes = { 'java' },
  cmd = { 'jdtls' },
  root_markers = {
    '.git',
    'build.gradle',
    'build.gradle.kts',
    'build.xml',
    'pom.xml',
    'settings.gradle',
    'settings.gradle.kts',
  },
}
