vim.filetype.add {
  extension = {
    mcfunction = 'mcfunction',
    idr = 'idris',
    lidr = 'lidris',
    fasta = 'fasta',
    kk = 'koka',
    drv = function(_, bufnr)
      local first_line = vim.api.nvim_buf_get_lines(bufnr, 0, 1, false)[1] or ''
      if
        -- https://nix.dev/manual/nix/2.25/protocols/derivation-aterm
        first_line:match '^Derive%(' or first_line:match '^DrvWithVersion%('
      then
        return 'nixdrv'
      end
    end,
    yaml = function(path, _)
      return path:match '%.dict%.yaml$' and 'rimedict' or 'yaml'
    end,
    ab = 'atob',
  },
  filename = {
    ['package.yaml'] = function(path, _)
      return vim.fn.findfile('stack.yaml', path .. ';') == ''
          and vim.fn.findfile('flake.nix', path .. ';') == ''
          and 'yaml'
        or 'yaml.stack'
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
