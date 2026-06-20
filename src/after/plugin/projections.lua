vim.cmd.packadd 'vim-projectionist'

-- TEST
-- vim.api.nvim_del_augroup_by_name 'projectionist_make'

-- vim-dispatch-like keymaps for :Console
vim.keymap.set('n', '<LocalLeader>\'<CR>', vim.cmd.Console)
vim.keymap.set('n', '<LocalLeader>\'<Space>', ':Console<Space>')
vim.keymap.set('n', '<LocalLeader>\'!', ':Console!<Space>')
vim.keymap.set('n', '<LocalLeader>\'?', function()
  local queries = vim.fn['projectionist#query'] 'console'
  local buf_local = vim.tbl_get(queries, 1, 2)
  if buf_local == nil then
    vim.notify('console not set', vim.log.levels.WARN)
  else
    if #queries > 1 then
      local project_local = vim.tbl_get(queries, #queries, 2)
      vim.notify(
        ('Buffer local is %s\nProject local is %s'):format(
          buf_local,
          project_local
        )
      )
    else
      vim.notify(('Buffer local is %s'):format(buf_local))
    end
  end
end)

vim.g.projectionist_heuristics = {
  -- nix flake
  ['flake.nix'] = {
    ['*'] = {
      console = [[nix repl --expr "builtins.getFlake \"$PWD\"" --keep-failed]],
      dispatch = 'nix build -L',
      make = 'nix',
      start = '-wait=always nix run',
    },
    ['flake.nix'] = {
      template = {
        '{open}',
        '  description = "***TODO {project|basename} flake description***";',
        '',
        '  inputs = {open}',
        '    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";',
        '    flake-utils.url = "github:numtide/flake-utils";',
        '  {close};',
        '',
        '  outputs =',
        '    {open}',
        '      self,',
        '      nixpkgs,',
        '      flake-utils,',
        '    {close}:',
        '    flake-utils.lib.eachDefaultSystem (',
        '      system:',
        '      let',
        '        pkgs = nixpkgs.legacyPackages.${open}system{close};',
        '      in',
        '      {open}',
        '        packages = rec {open}',
        '          {project|basename} = import ./default.nix {open} inherit pkgs; {close};',
        '          default = {project|basename};',
        '        {close};',
        '        apps = rec {open}',
        '          {project|basename} = flake-utils.lib.mkApp {open} drv = self.packages.${open}system{close}.{project|basename}; {close};',
        '          default = {project|basename};',
        '        {close};',
        '        devShells = {open}',
        '          default = import ./shell.nix {open} inherit pkgs; {close};',
        '        {close};',
        '      {close}',
        '    );',
        '{close}',
      },
      type = 'flake',
    },
    ['default.nix'] = {
      template = {
        '{open} pkgs ? import <nixpkgs> {open}{close} {close}:',
        '{open}',
        '{close}',
      },
      type = 'default',
    },
    ['shell.nix'] = {
      template = {
        '{open} pkgs ? import <nixpkgs> {open}{close} {close}:',
        'pkgs.mkShell {open}',
        '  packages = with pkgs; [',
        '  ];',
        '{close}',
      },
      type = 'shell',
    },
    ['*.nix'] = {
      template = {
        '{open} ... {close}:',
        '{open}',
        '{close}',
      },
    },
  },

  -- stack
  ['stack.yaml'] = {
    ['*'] = {
      console = 'stack ghci',
      dispatch = 'stack build',
      start = '-wait=always stack run',
      make = 'stack',
    },
    ['CHANGELOG.md'] = { type = 'changelog' },
    ['README.md'] = { type = 'readme' },
    ['package.yaml'] = { type = 'package' },
    -- '*' stands for '**/*'
    -- lhs not covered - FIX?
    ['src/*.hs'] = {
      type = 'src',
      template = { 'module {capitalize|dot} where' },
    },
    ['lib/*.hs'] = {
      type = 'lib',
      template = { 'module {capitalize|dot} where' },
    },
    ['app/*.hs'] = {
      type = 'app',
      template = { 'module {capitalize|dot} where' },
    },
  },

  -- cargo
  ['Cargo.toml'] = {
    ['*'] = {
      dispatch = 'cargo build',
      start = '-wait=always cargo run',
      make = 'cargo',
      console = 'evcxr',
    },
    ['src/*.rs'] = { type = 'src' },
  },

  -- maven
  ['pom.xml'] = {
    ['*'] = {
      dispatch = 'mvn compile',
      start = '-wait=always mvn exec:java',
      make = 'mvn',
      console = 'jshell',
    },
    ['src/main/java/*.java'] = {
      alternate = 'src/test/java/{}.java',
      template = {
        'package {dirname|dot};',
        '',
        'public class {basename} {open}{close}',
      },
      type = 'src',
    },
    ['src/test/java/*.java'] = {
      alternate = 'src/main/java/{}.java',
      template = {
        'package {dirname|dot};',
        '',
        'public class {basename} {open}{close}',
      },
      type = 'test',
    },
  },
}
