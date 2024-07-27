local maybe_stackyaml = function(path, _)
  -- A stack project always comes with (generated) cabal file
  if vim.fn.glob(vim.fs.joinpath(vim.fs.dirname(path), '*.cabal')) ~= '' then
    return 'stack.yaml'
  else
    return 'yaml'
  end
end

vim.filetype.add {
  filename = {
    ['package.yaml'] = maybe_stackyaml,
    ['stack.yaml'] = maybe_stackyaml,
    ['stack.yaml.lock'] = maybe_stackyaml,
    ['pom.xml'] = 'maven.xml',
  },
}
