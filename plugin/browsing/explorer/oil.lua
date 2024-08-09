local oil = require 'oil'
oil.setup {
  delete_to_trash = true,
  skip_confirm_for_simple_edits = true,
  watch_for_changes = true,
  win_options = { number = false },
  view_options = {
    show_hidden = false,
    is_hidden_file = function(name, _)
      return vim.tbl_contains(
        { '..', '.DS_Store', '.git', 'node_modules', '__pycache__', 'dist-newstyle' },
        name
      ) or vim
        .iter({ '.o', '.obj', '.dyn_hi', 'dyn_o', '.class', '.ibc', '.pyc', '.a', '.hi' })
        :any(function(suffix) return vim.endswith(name, suffix) end)
    end,
  },
}

local oil_toggle = function(path)
  local oil_windows = vim
    .iter(vim.fn.getwininfo())
    :filter(function(win) return vim.bo[win.bufnr].filetype == 'oil' end)

  if oil_windows:peek() then
    oil_windows:each(function(win) pcall(vim.api.nvim_win_close, win.winid, true) end)
  else
    vim.cmd 'topleft 40vsplit'
    oil.open(path)
  end
end

-- relative to file
vim.keymap.set('n', '<localleader>v', function() oil_toggle(vim.fn.expand '%:p:h') end)
-- cwd
vim.keymap.set('n', '<localleader>V', function() oil_toggle '.' end)
