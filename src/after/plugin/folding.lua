-- opts

vim.o.foldlevel = 99
vim.opt.fillchars:append { fold = ' ' }
vim.o.foldcolumn = '0'

-- toggle foldcolumn

vim.keymap.set('n', '<LocalLeader>z', function()
  if vim.wo.foldcolumn == '0' then
    vim.wo.foldcolumn = '2'
    vim.wo.signcolumn = 'no'
  else
    vim.wo.foldcolumn = '0'
    vim.wo.signcolumn = 'yes:1'
  end
end)

-- foldtext, based on
-- https://www.reddit.com/r/neovim/comments/1fzn1zt/custom_fold_text_function_with_treesitter_syntax

-- get hl group at coords (0-based)
-- if multiple hls are imposed, return last
-- if none, return nil
---@param buf integer
---@param lnum integer (1-based)
---@param col integer (1-based)
---@return string?
local get_hl = function(buf, lnum, col)
  return vim
    .iter(vim.treesitter.get_captures_at_pos(buf, lnum - 1, col - 1))
    :map(function(hl) return ('@%s.%s'):format(hl.capture, hl.lang) end)
    :last()
end

---@param buf integer non-zero
---@param lnum integer (1-based)
---@return Iter<string,string?>
local get_segments = function(buf, lnum)
  return vim
    .iter(ipairs(vim.split(vim.fn.getbufoneline(buf, lnum), '')))
    :map(function(col, char)
      local maybe_hl = get_hl(buf, lnum, col)
      return char, maybe_hl
    end)
end

-- vim-truthy
---@param value any
---@return boolean
local truthy = function(value)
  return value ~= nil and value ~= 0 and value ~= ''
end

-- custom foldtext like start line ... [endmarker]
-- endmarker displayed unless b:use_indent set to truthy value
---@return ([string,string]|[string])[]
Foldtext = function()
  require 'monkey-patches.iter' -- XXX Iter:dropwhile
  vim.api.nvim_set_hl(0, 'Folded', {}) -- no longer need that
  local buf = vim.api.nvim_get_current_buf()
  local start, finish = {}, {}
  local show_endmarker = not truthy(vim.b[buf].use_indent)
  start.segments = get_segments(buf, vim.v.foldstart):totable()
  finish.segments = show_endmarker
      and get_segments(buf, vim.v.foldend)
        :dropwhile(function(ch, _) return ch:match '%s' end)
        :totable()
    or nil

  return vim
    .iter({ start.segments, { { ' ... ', 'Comment' } }, finish.segments })
    :flatten()
    :totable()
end

vim.o.foldtext = 'v:lua.Foldtext()'
