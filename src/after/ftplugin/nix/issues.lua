-- open/search nixpkgs issues/PRs on GitHub repo

local issue = function(raw)
  local arg = vim.trim(raw)
  if arg == '' then vim.ui.open 'https://github.com/NixOS/nixpkgs/issues' end
  local id = arg:match '^#?(%d+)$'
  vim.ui.open(
    id and 'https://github.com/NixOS/nixpkgs/issues/' .. id
      or 'https://github.com/NixOS/nixpkgs/issues?q=' .. vim.uri_encode(arg)
  )
end

local pr = function(raw)
  local arg = vim.trim(raw)
  if arg == '' then vim.ui.open 'https://github.com/NixOS/nixpkgs/pulls' end
  local id = arg:match '^#?(%d+)$'
  vim.ui.open(
    id and 'https://github.com/NixOS/nixpkgs/pull/' .. id
      or 'https://github.com/NixOS/nixpkgs/pulls?q=' .. vim.uri_encode(arg)
  )
end

-- usage:
--   :Issue          open the issue list
--   :Issue #123     open issue/PR #123
--   :Issue 123      open issue/PR #123
--   :Issue yabai    search for "yabai"
vim.api.nvim_buf_create_user_command(
  0,
  'Issue',
  function(o) issue(o.args) end,
  { nargs = '?', desc = 'Open/search nixpkgs GitHub issues' }
)

-- usage:
--   :PR            open the PR list
--   :PR #123       open issue/PR #123
--   :PR 123        open issue/PR #123
--   :PR stdenv     search for "stdenv"
vim.api.nvim_buf_create_user_command(
  0,
  'PR',
  function(o) pr(o.args) end,
  { nargs = '?', desc = 'Open/search nixpkgs GitHub PRs' }
)

-- use the keyword under cursor/selected
vim.keymap.set('n', '<Space>#', function() issue(vim.fn.expand '<cWORD>') end, {
  buffer = 0,
  desc = 'Open/search nixpkgs issue under cursor',
})
vim.keymap.set(
  'x',
  '<Space>#',
  function()
    issue(
      vim
        .iter(
          vim.fn.getregion(
            vim.fn.getpos '.',
            vim.fn.getpos 'v',
            { type = vim.fn.mode() }
          )
        )
        :map(vim.trim)
        :join ' '
    )
  end,
  { buffer = 0, desc = 'Open/search nixpkgs issue under cursor' }
)
