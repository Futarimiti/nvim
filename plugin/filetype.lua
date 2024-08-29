vim.filetype.add {
  extension = {
    idr = 'idris',
    lidr = 'lidris',
  },
  filename = {
    ['package.yaml'] = function(path, _)
      return vim.fn.findfile('stack.yaml', path .. ';') == '' and 'yaml' or 'yaml.stack'
    end,
    ['stack.yaml'] = 'yaml.stack',
    ['stack.yaml.lock'] = 'yaml.stack',
    ['pom.xml'] = 'xml.maven',
    ['Cargo.lock'] = 'toml.cargo',
    ['Cargo.toml'] = 'toml.cargo',
    ['lakefile.lean'] = 'lean.lake',
    ['lean-toolchain'] = 'leantoolchain',
    Brewfile = 'ruby',
  },
}
