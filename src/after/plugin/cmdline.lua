vim.o.inccommand = 'split'

-- capturing command output
-- https://www.reddit.com/r/neovim/comments/1g1xyi3/capture_the_command_output/

vim.keymap.set('n', 'y:', function()
  vim.ui.input({ prompt = '(yank) :', completion = 'command' }, function(input)
    if input == '' or input == nil then return end
    local output = vim.api.nvim_exec2(input, { output = true }).output
    vim.fn.setreg(vim.v.register, output)
  end)
end)
vim.keymap.set('n', 'y!', 'y:!', { remap = true })

-- opens a window (height references `cmdwinheight`) with cmd output
vim.keymap.set('n', '<C-W>:', function()
  vim.ui.input(
    { prompt = '(capture) :', completion = 'command' },
    function(input)
      if input == '' or input == nil then return end
      local output = vim.api.nvim_exec2(input, { output = true }).output
      local buf = vim.api.nvim_create_buf(false, true)
      vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(output, '\n'))
      local win = vim.api.nvim_open_win(buf, true, {
        height = vim.o.cmdwinheight,
        split = 'below',
        win = 0,
      })
      -- escape % signs - used by 'statusline' as interpolation
      vim.wo[win].statusline = ':' .. (input:gsub('%%', '%%%%'))
    end
  )
end)
vim.keymap.set('n', '<C-W>!', '<C-W>:!', { remap = true })

-- command aliases

vim.cmd.packadd 'cmdalias.vim'
vim
  .iter({
    man = 'Man',
    rm = '!rm',
    mv = '!mv',
  })
  :each(vim.fn.CmdAlias)

-- ...yeah

vim.keymap.set({ 'n', 'x' }, '：', ':')
