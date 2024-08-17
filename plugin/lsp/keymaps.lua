vim.keymap.set('n', '<C-K>', vim.lsp.buf.hover)
vim.keymap.set('n', '<C-CR>', vim.lsp.buf.code_action)
vim.keymap.set('n', 'gi', vim.lsp.buf.implementation)
vim.keymap.set('n', '<leader><leader>', vim.lsp.buf.rename)
vim.keymap.set('n', '<leader>\\', vim.lsp.buf.rename)
vim.keymap.set('n', '<leader>,', vim.lsp.buf.rename)
vim.keymap.set('n', '<leader>-', vim.lsp.buf.rename)

vim.api.nvim_create_autocmd('LspAttach', {
  desc = 'capability keymaps upon LSP attach',
  group = vim.api.nvim_create_augroup('lsp-capability-keymaps', {}),
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client.supports_method(vim.lsp.protocol.Methods.textDocument_definition) then
      vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { buffer = args.buf })
    end
    if client.supports_method(vim.lsp.protocol.Methods.textDocument_typeDefinition) then
      vim.keymap.set('n', 'gD', vim.lsp.buf.type_definition, { buffer = args.buf })
    end
    if client.supports_method(vim.lsp.protocol.Methods.textDocument_codeAction) then
      vim.keymap.set('n', '<CR>', vim.lsp.buf.code_action, { buffer = args.buf })
    end
    if client.supports_method(vim.lsp.protocol.Methods.textDocument_references) then
      vim.keymap.set('n', 'gr', vim.lsp.buf.references, { buffer = args.buf, nowait = true })
    end
  end,
})
