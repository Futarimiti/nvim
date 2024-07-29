vim.filetype.add {
  filename = {
    ['package.yaml'] = function(path, _)
      return vim.fn.findfile('stack.yaml', '.') == '' and 'yaml' or 'stack.yaml'
    end,
    ['stack.yaml'] = 'stack.yaml',
    ['stack.yaml.lock'] = 'stack.yaml',
    ['pom.xml'] = 'maven.xml',
  },
}
