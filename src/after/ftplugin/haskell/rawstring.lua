-- rawstring closing quote highlight
-- escape sequence: |~]

vim.api.nvim_set_hl(0, 'RQClosingQuote', { link = 'Special' })

local ns = vim.api.nvim_create_namespace 'haskell-rawstring'
local buf = vim.api.nvim_get_current_buf()

local highlight_interpolates = function()
  vim.api.nvim_buf_clear_namespace(buf, ns, 0, -1)
  local parser = vim.treesitter.get_parser(buf, 'haskell')
  local tree = parser:parse()[1]
  local root = tree:root()
  local query = vim.treesitter.query.parse(
    'haskell',
    [[(quasiquote
        (quoter) @interpolator
        (#eq? @interpolator "rQ")
        (quasiquote_body) @body)]]
  )

  vim.iter(query:iter_matches(root, buf)):each(function(_, node, _)
    local body_node = node[2][1]
    local start_row, start_col, _, _ = body_node:range()
    local text = vim.treesitter.get_node_text(body_node, buf)

    require 'monkey-patches.string' -- XXX string:find_all
    vim.iter(text:find_all '%|%~+%]'):each(function(indices)
      local escape_start, escape_end = unpack(indices)
      local buf_start_col = start_col + escape_start - 1
      local buf_end_col = start_col + escape_end - 1
      vim.hl.range(
        buf,
        ns,
        'RQClosingQuote',
        { start_row, buf_start_col },
        { start_row, buf_end_col },
        { inclusive = true }
      )
    end)
  end)
end

vim.api.nvim_create_autocmd(
  { 'BufEnter', 'TextChanged', 'TextChangedI', 'InsertLeave' },
  {
    buffer = buf,
    callback = highlight_interpolates,
    group = vim.api.nvim_create_augroup('haskell-rawstring-escape', {}),
  }
)
