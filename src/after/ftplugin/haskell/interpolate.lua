-- XXX ts parser currently broken
if true then return end

-- variable interpolation highlights

vim.api.nvim_set_hl(0, 'InterpolateContent', { link = 'Special' })
vim.api.nvim_set_hl(0, 'InterpolateOpeningBrace', { link = 'Special' })
vim.api.nvim_set_hl(0, 'InterpolateClosingBrace', { link = 'Special' })

local ns = vim.api.nvim_create_namespace 'haskell-interpolates'
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
        (#any-of? @interpolator "i" "iii" "__i" "__i'E" "__i'L" "iii'E" "iii'L")
        (quasiquote_body) @body)]]
  )

  vim.iter(query:iter_matches(root, buf)):each(function(_, node, _)
    local body_node = node[2][1]
    local start_row, start_col, _, _ = body_node:range()
    local text = vim.treesitter.get_node_text(body_node, buf)

    require 'monkey-patches.string' -- XXX string:find_all
    vim.iter(text:find_all '#{.-}'):each(function(indices)
      local interpolate_start, interpolate_end = unpack(indices)
      local buf_start_col = start_col + interpolate_start - 1
      local buf_end_col = start_col + interpolate_end - 1
      vim.hl.range(
        buf,
        ns,
        'InterpolateOpeningBrace',
        { start_row, buf_start_col },
        { start_row, buf_start_col + 2 }
      )

      vim.hl.range(
        buf,
        ns,
        'InterpolateClosingBrace',
        { start_row, buf_end_col },
        { start_row, buf_end_col + 1 }
      )

      vim.hl.range(
        buf,
        ns,
        'InterpolateContent',
        { start_row, buf_start_col + 2 },
        { start_row, buf_end_col }
      )
    end)
  end)
end

vim.api.nvim_create_autocmd(
  { 'BufEnter', 'TextChanged', 'TextChangedI', 'InsertLeave' },
  {
    buffer = buf,
    callback = highlight_interpolates,
    group = vim.api.nvim_create_augroup('haskell-interpolate-string', {}),
  }
)
