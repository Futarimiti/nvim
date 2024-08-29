local oil = require 'oil'

local open = function(path)
  local win = vim.w.open_from_win or 0
  win = vim.api.nvim_win_is_valid(win) and win or 0
  vim.api.nvim_set_current_win(win)
  vim.cmd.edit { path, mods = { confirm = true } }
end

-- helper function to parse output
local parse_output = function(proc)
  local result = proc:wait()
  local ret = {}
  if result.code == 0 then
    for line in vim.gsplit(result.stdout, '\n', { plain = true, trimempty = true }) do
      -- Remove trailing slash
      line = line:gsub('/$', '')
      ret[line] = true
    end
  end
  return ret
end

-- build git status cache
local new_git_status = function()
  return setmetatable({}, {
    __index = function(self, key)
      local ignore_proc = vim.system(
        { 'git', 'ls-files', '--ignored', '--exclude-standard', '--others', '--directory' },
        {
          cwd = key,
          text = true,
        }
      )
      local tracked_proc = vim.system({ 'git', 'ls-tree', 'HEAD', '--name-only' }, {
        cwd = key,
        text = true,
      })
      local ret = {
        ignored = parse_output(ignore_proc),
        tracked = parse_output(tracked_proc),
      }

      rawset(self, key, ret)
      return ret
    end,
  })
end
local git_status = new_git_status()

-- Clear git status cache on refresh
local refresh = require('oil.actions').refresh
local orig_refresh = refresh.callback
refresh.callback = function(...)
  git_status = new_git_status()
  orig_refresh(...)
end

oil.setup {
  -- default_file_explorer = false, -- still need netrw for some use
  delete_to_trash = true,
  skip_confirm_for_simple_edits = true,
  watch_for_changes = true,
  win_options = { number = false, signcolumn = 'yes:1' },
  view_options = {
    show_hidden = false,
    is_hidden_file = function(name, _)
      local dir = oil.get_current_dir()
      -- if no local directory (e.g. for ssh connections), give up
      if not dir then return false end
      -- Check if file is gitignored
      return git_status[dir].ignored[name]
        or vim.tbl_contains(
          { '..', '.DS_Store', '.git', 'node_modules', '__pycache__', 'dist-newstyle' },
          name
        )
        or vim
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
        if entry == nil then return end
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
-- if not on a normal file, fallback to cwd
vim.keymap.set('n', '<localleader>v', function()
  local oil_windows = vim.iter(vim.api.nvim_tabpage_list_wins(0)):filter(
    function(win) return vim.filetype.match { buf = vim.api.nvim_win_get_buf(win) } == 'oil' end
  )

  if oil_windows:peek() then
    oil_windows:each(function(win) pcall(vim.api.nvim_win_close, win, true) end)
  else
    local open_from = vim.api.nvim_get_current_win()
    vim.cmd 'topleft 35vsplit'
    local filename = vim.fn.expand '%:p:h'
    oil.open(filename:match '://' and '.' or filename)
    vim.w.open_from_win = open_from
  end
end)
-- cwd
vim.keymap.set('n', '<localleader>V', function()
  vim
    .iter(vim.api.nvim_tabpage_list_wins(0))
    :filter(
      function(win) return vim.filetype.match { buf = vim.api.nvim_win_get_buf(win) } == 'oil' end
    )
    :each(function(win) pcall(vim.api.nvim_win_close, win, true) end)
  local open_from = vim.api.nvim_get_current_win()
  vim.cmd 'topleft 35vsplit'
  oil.open '.'
  vim.w.open_from_win = open_from
end)
