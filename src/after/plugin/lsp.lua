vim.lsp.config('*', { root_markers = { '.git' } })
vim.lsp.enable {
  'luals',
  'texlab',
  'ruff',
  'basedpyright-no-diagnostics',
  'nixd',
}

-- commands

-- quick enable & disable

local lsp_compl = function(arglead, _, _)
  return vim
    .iter(vim.api.nvim_get_runtime_file('lsp/*.lua', true))
    :map(function(f) return vim.fn.fnamemodify(f, ':t:r') end)
    :filter(function(name) return vim.startswith(name, arglead) end)
    :totable()
end

vim.api.nvim_create_user_command(
  'LspEnable',
  function(o) vim.lsp.enable(o.fargs) end,
  { desc = 'enable lsp clients', nargs = '+', complete = lsp_compl }
)

-- need to re-enter buffer or open in a new buffer
vim.api.nvim_create_user_command(
  'LspDisable',
  function(o) vim.lsp.enable(o.fargs, false) end,
  { desc = 'disable lsp clients', nargs = '+', complete = lsp_compl }
)

-- keymaps

-- available regardless of LSP
vim.keymap.set({ 'n', 'x' }, '<C-K>', vim.lsp.buf.hover)
vim.keymap.set({ 'n', 'x' }, '<C-CR>', vim.lsp.buf.code_action)
vim.keymap.set({ 'n', 'x' }, 'gi', vim.lsp.buf.implementation)
vim.keymap.set('n', [[\\]], vim.lsp.buf.rename)
vim.keymap.set('n', ',,', vim.lsp.buf.rename)

-- keymaps augmented by LSP
vim.api.nvim_create_autocmd('LspAttach', {
  desc = 'capability keymaps upon LSP attach',
  group = vim.api.nvim_create_augroup('lsp-capability-keymaps', {}),
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if
      client:supports_method(vim.lsp.protocol.Methods.textDocument_definition)
    then
      vim.keymap.set(
        { 'n', 'x' },
        'gd',
        vim.lsp.buf.definition,
        { buffer = args.buf }
      )
    end
    if
      client:supports_method(
        vim.lsp.protocol.Methods.textDocument_typeDefinition
      )
    then
      vim.keymap.set(
        { 'n', 'x' },
        'gD',
        vim.lsp.buf.type_definition,
        { buffer = args.buf }
      )
    end
    if
      client:supports_method(vim.lsp.protocol.Methods.textDocument_codeAction)
    then
      vim.keymap.set(
        { 'n', 'x' },
        '<CR>',
        vim.lsp.buf.code_action,
        { buffer = args.buf }
      )
    end
    if
      client:supports_method(vim.lsp.protocol.Methods.textDocument_references)
    then
      vim.keymap.set(
        { 'n', 'x' },
        'gr',
        vim.lsp.buf.references,
        { buffer = args.buf, nowait = true }
      )
    end
  end,
})
