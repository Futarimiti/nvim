if vim.fn.exepath 'lua-language-server' == '' then return end

local bufname = vim.api.nvim_buf_get_name(0)
if
  vim.startswith(bufname, vim.fn.stdpath 'config')
  or vim.startswith(bufname, os.getenv 'VIMRUNTIME')
then
  vim.lsp.start {
    cmd = { 'lua-language-server' },
    name = 'lua_ls', -- do not change this - lazydev searches for exact "lua_ls"
    root_dir = vim.fn.stdpath 'config',
    settings = {
      Lua = {
        completion = {
          callSnippet = 'Disable',
          keywordSnippet = 'Disable',
        },
        diagnostics = {
          disable = {
            'need-check-nil',
            'missing-parameter',
            'cast-local-type',
            'missing-fields',
          },
          enable = true,
        },
        format = { enable = false },
        hint = {
          enable = true,
          setType = true,
        },
        semantic = { enable = false },
        telemetry = { enable = false },
        type = {
          castNumberToInteger = true,
          weakNilCheck = true,
          weakUnionCheck = true,
        },
        workspace = {
          maxPreload = 2000,
          preloadFileSize = 1000,
        },
      },
    },
  }
  vim.cmd 'packadd! lazydev'

  require('lazydev').setup {
    integrations = {
      lspconfig = false,
      cmp = false,
      coq = false,
    },
    library = {
      { path = 'luvit-meta/library', words = { 'vim%.uv' } },
    },
  }
else
  vim.lsp.start {
    cmd = { 'lua-language-server' },
    name = 'lua-ls',
    root_dir = vim.fs.root(0, { '.luarc.json', '.git' }),
    settings = {
      Lua = {
        completion = {
          callSnippet = 'Disable',
          keywordSnippet = 'Disable',
        },
        format = { enable = false },
        hint = {
          enable = true,
          setType = true,
        },
        semantic = { enable = false },
        telemetry = { enable = false },
        type = {
          castNumberToInteger = true,
          weakNilCheck = true,
          weakUnionCheck = true,
        },
      },
    },
  }
end
