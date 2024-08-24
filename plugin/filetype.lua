vim.filetype.add {
  extension = {
    idr = 'idris',
    lidr = 'lidris',
  },
  filename = {
    ['package.yaml'] = function(path, _)
      return vim.fn.findfile(path .. ';', '.') == '' and 'yaml' or 'stack.yaml'
    end,
    ['stack.yaml'] = 'stack.yaml',
    ['stack.yaml.lock'] = 'stack.yaml',
    ['pom.xml'] = 'maven.xml',
    ['Cargo.lock'] = 'cargo.toml',
    ['Cargo.toml'] = 'cargo.toml',
    ['lakefile.lean'] = 'lean.lake',
    ['lean-toolchain'] = 'leantoolchain',
    Brewfile = 'ruby',
  },
}
