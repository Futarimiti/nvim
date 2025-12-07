-- insert mode autocomplete
local group = vim.api.nvim_create_augroup('ins-autocomplete', {})

-- given 'completion' = bringing up the completion popup menu:
-- when typing too quickly and/or vim scanning too many words for completion
-- the completion trigger may fire again before previous completion has finished
-- resulting in double-feeds of <C-N> or whatever which is bad
-- so we need this (ugly) lock variable to track if there's already a completion
-- in progress and kill any attempts to trigger another completion if yes
-- a timer should be a less coarse solution but it works so touch it not
local complete_in_progress = false

vim.api.nvim_create_autocmd('InsertCharPre', {
  desc = 'filepath & lsp & keyword completion',
  group = group,
  callback = function(args)
    if
      complete_in_progress
      or vim.fn.pumvisible() ~= 0 -- visible
      or vim.tbl_contains(
        { 'terminal', 'prompt', 'help' },
        vim.bo[args.buf].buftype
      )
    then
      return
    end

    complete_in_progress = true -- lock

    if vim.v.char == '/' then
      vim.api.nvim_feedkeys(vim.keycode '<C-X><C-F>', 'ni', false)
    elseif
      not vim.tbl_isempty(vim.lsp.get_clients {
        bufnr = args.buf,
        method = vim.lsp.protocol.Methods.textDocument_completion,
      }) -- has completion-capable lsp(s) attached
    then
      -- print '[DEBUG] get!'
      vim.lsp.completion.get()
    elseif
      vim.fn.match(vim.v.char, [[\k]]) ~= -1 -- inserted keyword
    then
      vim.api.nvim_feedkeys(vim.keycode '<C-N>', 'ni', false)
    end
  end,
})

-- (with our approach) the input character won't be displayed
-- (e.g. TextChanged events won't be trigger) until candidates are found
-- therefore we can confidently say completion has finished
-- and pum has shown up upon text change
vim.api.nvim_create_autocmd({ 'TextChangedP', 'TextChangedI' }, {
  desc = 'reset complete_in_progress lock',
  group = group,
  callback = function() complete_in_progress = false end,
})

-- upon attaching lsp to a buffer, check completion capability
-- if capable, enable lsp completion with autotrigger
vim.api.nvim_create_autocmd('LspAttach', {
  desc = 'auto enable lsp completion if capable',
  group = group,
  callback = function(args)
    local client_id = args.data.client_id
    local client = vim.lsp.get_client_by_id(client_id)
    if
      client:supports_method(vim.lsp.protocol.Methods.textDocument_completion)
    then
      vim.lsp.completion.enable(
        true,
        client_id,
        args.buf,
        { autotrigger = true }
      )
    end
  end,
})
