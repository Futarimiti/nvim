vim.keymap.set(
  { 'i', 'c' },
  '<F5>',
  function() return os.date '%Y-%m-%d %H:%M:%S' end,
  { expr = true }
)

vim.keymap.set(
  { 'i', 'c' },
  '<F17>', -- shift + <F5>
  function() return os.date '%Y-%m-%d' end,
  { expr = true }
)
