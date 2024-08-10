local oil = require 'oil'

local open = function(path)
  local win = vim.w.open_from_win or 0
  vim.api.nvim_win_call(win, function() vim.cmd.edit(path) end)
  vim.api.nvim_set_current_win(win)
end

local oil_toggle = function(path)
  local oil_windows = vim
    .iter(vim.fn.getwininfo())
    :filter(function(win) return vim.bo[win.bufnr].filetype == 'oil' end)

  if oil_windows:peek() then
    oil_windows:each(function(win) pcall(vim.api.nvim_win_close, win.winid, true) end)
  else
    local open_from = vim.api.nvim_get_current_win()
    vim.cmd 'topleft 35vsplit'
    oil.open(path)
    vim.w.open_from_win = open_from
  end
end

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
  keymaps = {
    ['<CR>'] = {
      mode = 'n',
      desc = 'Open the entry under the cursor in the window that starts oil',
      callback = function()
        local entry = oil.get_cursor_entry()
        if entry.type == 'directory' then
          require('oil.actions').select.callback()
        elseif entry.type == 'link' then
          local linkto = entry.meta.link
          local path = vim.fs.normalize(linkto)
          if vim.fn.isdirectory(path) then
            require('oil.actions').select.callback()
          else
            open(path)
          end
        else
          -- .name or .parsed_name?
          local tail = entry.name
          local cwd, _ = vim.api.nvim_buf_get_name(0):gsub('^oil://', '')
          local path = vim.fs.joinpath(cwd, tail)
          open(path)
        end
      end,
    },
  },
}

-- relative to file
vim.keymap.set('n', '<localleader>v', function() oil_toggle(vim.fn.expand '%:p:h') end)
-- cwd
vim.keymap.set('n', '<localleader>V', function() oil_toggle '.' end)
