vim.opt.completeopt = { 'menuone', 'noselect' }

local autocomplete_group = vim.api.nvim_create_augroup('simple-autocomplete', {})

local autocomplete_in_progress = false

vim.api.nvim_create_autocmd('InsertCharPre', {
  desc = 'trigger autocomplete based on character inserted',
  group = autocomplete_group,
  callback = function(args)
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

    local inserted_trigger_character = vim
      .iter(vim.lsp.get_clients {
        bufnr = args.buf,
        method = vim.lsp.protocol.Methods.textDocument_completion,
      })
      :map(
        function(client)
          return vim.tbl_get(
            client,
            'server_capabilities',
            'completionProvider',
            'triggerCharacters'
          )
        end
      )
      :any(function(trigger_characters) return vim.tbl_contains(trigger_characters, vim.v.char) end)
    if inserted_trigger_character then
      local omnifunc = vim.bo.omnifunc
      -- XXX change this mess to vim.lsp.completion.trigger after becoming stable
      vim.bo.omnifunc = 'v:lua.vim.lsp.omnifunc'
      vim.api.nvim_feedkeys(vim.keycode '<C-X><C-O>', 'ni', false)
      vim.schedule(function() vim.bo.omnifunc = omnifunc end)
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
  desc = 'restore autocomplete',
  group = autocomplete_group,
  callback = function() autocomplete_in_progress = false end,
})
