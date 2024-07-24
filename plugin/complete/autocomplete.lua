local autocomplete_group = vim.api.nvim_create_augroup('autocomplete', {})

local autocomplete_in_progress = false

vim.api.nvim_create_autocmd('LspAttach', {
  desc = 'lsp autocomplete',
  group = autocomplete_group,
  callback = function(args)
    if
      vim.lsp
        .get_client_by_id(args.data.client_id)
        .supports_method(vim.lsp.protocol.Methods.textDocument_completion)
    then
      vim.lsp.completion.enable(true, args.data.client_id, args.buf, { autotrigger = true })
    end
  end,
})

vim.api.nvim_create_autocmd('InsertCharPre', {
  desc = 'keyword & filepath autocomplete',
  group = autocomplete_group,
  callback = function()
    if
      autocomplete_in_progress
      or vim.fn.pumvisible() == 1
      or vim.fn.state 'm' == 'm'
      or vim.bo.buftype ~= ''
    then
      return
    end

    autocomplete_in_progress = true

    if vim.v.char == '/' then
      vim.api.nvim_feedkeys(vim.keycode '<C-X><C-F>', 'ni', false)
      return
    end

    local iskeyword = vim.bo.iskeyword
    vim.opt_local.iskeyword:append(vim.b.autocomplete_extra_isk or '')
    local inserted_keyword = vim.fn.match(vim.v.char, [[\k]]) ~= -1
    vim.schedule(function() vim.bo.iskeyword = iskeyword end)
    if inserted_keyword then
      vim.api.nvim_feedkeys(vim.keycode '<C-N>', 'ni', false)
      return
    end
  end,
})

vim.api.nvim_create_autocmd({ 'TextChangedP', 'TextChangedI' }, {
  desc = 'finish autocomplete',
  group = autocomplete_group,
  callback = function() autocomplete_in_progress = false end,
})
