vim.cmd.packadd 'oil.nvim'

-- check if the file matches any wildignore pattern;
local is_wildignored = function(path)
  return vim.iter(vim.opt.wildignore:get()):any(
    function(pat) return vim.fn.match(path, vim.fn.glob2regpat(pat)) >= 0 end
  )
end

local oil = require 'oil'

oil.setup {
  delete_to_trash = false,
  win_options = {
    signcolumn = vim.go.signcolumn,
  },
  skip_confirm_for_simple_edits = true,
  constrain_cursor = 'name',
  watch_for_changes = false,
  keymaps = {
    ['<C-p>'] = false,
    ['<C-c>'] = false,
    ['<C-s>'] = false,
    ['<C-h>'] = false,
    ['<C-t>'] = false,
    ['_'] = false,
    ['`'] = false,
    ['g~'] = false,
    ['gs'] = false,
    ['g\\'] = false,

    ['y.'] = 'actions.yank_entry',
    ['<Space>p'] = { 'actions.preview', opts = { horizontal = true } },
    ['<2-LeftMouse>'] = 'actions.select',

    -- NOTE these below are implemented in oil ftplugin
    -- it's just left here so desc appears in g? popup

    -- 'actions.open_cmdline' would only work in normal mode for one entry only
    -- these extend to visual mode to select multiple entries as well
    ['.'] = { desc = 'Send current or selected entries to commandline' },
    ['!'] = { desc = 'Equivalent of \'.!\'' },
  },
  view_options = {
    show_hidden = false,
    is_hidden_file = function(name, _)
      return is_wildignored(name)
      -- not checking git since not all ignoring files are intended to be hidden
    end,
    is_always_hidden = function(name, _)
      -- no need to display ../
      return name == '..'
    end,
  },
}

vim.keymap.set('n', '-', oil.open)
