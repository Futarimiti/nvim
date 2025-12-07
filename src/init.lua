-- must turn on NOW - otherwise too late to pick up exrcs
vim.o.exrc = true

-- neovide

if vim.g.neovide ~= nil then
  vim.g.neovide_transparency = 0.85
  vim.g.neovide_scroll_animation_length = 0.1
  vim.g.neovide_position_animation_length = 0.05
  vim.g.neovide_input_macos_option_key_is_meta = 'both'
  vim.g.neovide_cursor_trail_size = 0.5
end

-- leaders

vim.g.mapleader = '\\'
vim.g.maplocalleader = ' '
vim.keymap.set({ 'n', 'x', 'o' }, ',', '\\', { remap = true })
