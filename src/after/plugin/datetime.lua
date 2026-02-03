vim.keymap.set(
  { 'i', 'c' },
  '<F5>',
  function() return os.date '%Y-%m-%d %H:%M:%S' end,
  { expr = true }
)
