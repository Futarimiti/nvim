vim.opt.completeopt = { 'menuone', 'noselect' }
-- uncomment after fuzzy-matching support for insert-completion becomes stable
-- vim.opt.completeopt:append 'fuzzy'

local autocomplete_group = vim.api.nvim_create_augroup('autocomplete', {})

local autocomplete_in_progress = false

vim.api.nvim_create_autocmd('InsertCharPre', {
  desc = 'trigger autocomplete based on character inserted',
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

    local inserted_trigger_character =
      vim.tbl_contains(vim.b.lsp_trigger_characters or {}, vim.v.char)

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

vim.api.nvim_create_autocmd('LspAttach', {
  desc = 'cache lsp trigger_characters into vim.b.lsp_trigger_characters',
  group = autocomplete_group,
  callback = function(args)
    vim.b.lsp_trigger_characters = vim
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
      :flatten()
      :totable()
  end,
})

vim.api.nvim_create_autocmd('LspDetach', {
  desc = 'clear lsp trigger_characters cache in vim.b.lsp_trigger_characters upon lsp detach',
  group = autocomplete_group,
  callback = function(args)
    vim.schedule(function()
      if
        vim.tbl_isempty(vim.lsp.get_clients {
          bufnr = args.buf,
          method = vim.lsp.protocol.Methods.textDocument_completion,
        })
      then
        vim.b.lsp_trigger_characters = nil
      end
    end)
  end,
})
