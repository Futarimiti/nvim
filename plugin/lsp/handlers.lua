-- check $VIMRUNTIME/lua/vim/lsp/handlers.lua

vim.lsp.handlers[vim.lsp.protocol.Methods.textDocument_hover] = function(_, result, ctx, config)
  config = config or {}
  config.focus_id = ctx.method
  if vim.api.nvim_get_current_buf() ~= ctx.bufnr then
    -- Ignore result since buffer changed. This happens for slow language servers.
    return
  end
  if not (result and result.contents) then
    if not config.silent then vim.notify 'No hover information available' end
    return
  end
  local contents ---@type string[]
  if type(result.contents) == 'table' and result.contents.kind == 'plaintext' then
    contents = vim.split(result.contents.value or '', '\n', { trimempty = true })
  else
    contents = vim.lsp.util.convert_input_to_markdown_lines(result.contents)
  end
  if vim.tbl_isempty(contents) then
    if not config.silent then vim.notify 'No hover information available' end
    return
  end
  local buf = vim.api.nvim_create_buf(false, true)
  vim.lsp.util.stylize_markdown(buf, contents, {})
  vim.bo[buf].syntax = 'OFF'
  vim.bo[buf].keywordprg = ':help'
  vim.bo[buf].bufhidden = 'wipe'
  vim.cmd 'pclose!'
  local win = vim.api.nvim_open_win(buf, false, {
    height = vim.o.previewheight,
    split = 'above',
    win = 0,
  })
  vim.wo[win].previewwindow = true
  vim.wo[win].conceallevel = 3
  vim.wo[win].foldenable = false
  vim.wo[win].winfixbuf = true
  vim.wo[win].wrap = true
  vim.wo[win].statusline = '[LSP] vim.lsp.buf.hover'
end

vim.lsp.handlers[vim.lsp.protocol.Methods.textDocument_signatureHelp] = function(
  _,
  result,
  ctx,
  config
)
  config = config or {}
  config.focus_id = ctx.method
  if vim.api.nvim_get_current_buf() ~= ctx.bufnr then
    -- Ignore result since buffer changed. This happens for slow language servers.
    return
  end
  -- When use `autocmd CompleteDone <silent><buffer> lua vim.lsp.buf.signature_help()` to call signatureHelp handler
  -- If the completion item doesn't have signatures It will make noise. Change to use `print` that can use `<silent>` to ignore
  if not (result and result.signatures and result.signatures[1]) then
    if not config.silent then print 'No signature help available' end
    return
  end
  local client = assert(vim.lsp.get_client_by_id(ctx.client_id))
  local triggers =
    vim.tbl_get(client.server_capabilities, 'signatureHelpProvider', 'triggerCharacters')
  local ft = vim.bo[ctx.bufnr].filetype
  local lines, hl = vim.lsp.util.convert_signature_help_to_markdown_lines(result, ft, triggers)
  if not lines or vim.tbl_isempty(lines) then
    if not config.silent then print 'No signature help available' end
    return
  end
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].filetype = 'markdown'
  vim.bo[buf].keywordprg = ':help'
  vim.bo[buf].bufhidden = 'wipe'
  vim.treesitter.start(buf)
  vim.cmd 'pclose!'
  local win = vim.api.nvim_open_win(buf, false, {
    height = vim.o.previewheight,
    split = 'above',
    win = 0,
  })
  vim.wo[win].previewwindow = true
  vim.wo[win].conceallevel = 3
  vim.wo[win].foldenable = false
  vim.wo[win].winfixbuf = true
  vim.wo[win].wrap = true
  vim.wo[win].statusline = '[LSP] vim.lsp.buf.signature_help'
  if hl then
    -- Highlight the second line if the signature is wrapped in a Markdown code block.
    local line = vim.startswith(lines[1], '```') and 1 or 0
    vim.api.nvim_buf_add_highlight(buf, -1, 'LspSignatureActiveParameter', line, unpack(hl))
  end
end
