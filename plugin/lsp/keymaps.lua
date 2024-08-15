vim.keymap.set('n', '<C-K>', vim.lsp.buf.hover)
vim.keymap.set('i', '<C-S>', vim.lsp.buf.signature_help)
vim.keymap.set('n', '<C-CR>', vim.lsp.buf.code_action)
vim.keymap.set('n', 'gr', vim.lsp.buf.references)
vim.keymap.set('n', 'gi', vim.lsp.buf.implementation)
vim.keymap.set('n', '<leader><leader>', vim.lsp.buf.rename)
vim.keymap.set('n', '<leader>\\', vim.lsp.buf.rename)
vim.keymap.set('n', '<leader>,', vim.lsp.buf.rename)
vim.keymap.set('n', '<leader>-', vim.lsp.buf.rename)

vim.api.nvim_create_autocmd('LspAttach', {
  desc = 'capability keymaps upon LSP attach',
  group = vim.api.nvim_create_augroup('lsp-capability-keymaps', {}),
  callback = function(args)
    local capabilities = vim.lsp.get_client_by_id(args.data.client_id).server_capabilities
    if capabilities.definitionProvider then
      vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { buffer = args.buf })
    end
    if capabilities.typeDefinitionProvider then
      vim.keymap.set('n', 'gD', vim.lsp.buf.type_definition, { buffer = args.buf })
    end
    if capabilities.hoverProvider and vim.bo.keywordprg == '' then
      vim.keymap.set('n', 'K', vim.lsp.buf.hover, { buffer = args.buf })
    end
    if capabilities.codeActionProvider then
      vim.keymap.set('n', '<CR>', vim.lsp.buf.code_action, { buffer = args.buf })
    end
  end,
})
