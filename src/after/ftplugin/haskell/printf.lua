-- XXX ts parser currently broken
if true then return end

-- printf format specifier highlights

local buf = vim.api.nvim_get_current_buf()
local ns = vim.api.nvim_create_namespace 'haskell-printf-specifiers'
vim.api.nvim_set_hl(0, 'FormatSpecifier', { link = 'Special' })

-- ref: https://hackage.haskell.org/package/base-4.21.0.0/docs/Text-Printf.html#v:printf
local pattern = '%%[%-%+%s0#]*%d*%*?%.?%d*%*?[hlL]?[hl]?[cdoxXbufFgGeEsv]'

local highlight_format_specifiers = function()
  vim.api.nvim_buf_clear_namespace(buf, ns, 0, -1)

  local parser = vim.treesitter.get_parser(buf, 'haskell')
  local tree = parser:parse()[1]
  local root = tree:root()

  local query = vim.treesitter.query.parse(
    'haskell',
    [[(apply
        function: (variable) @printf
        (#eq? @printf "printf")
        argument: (literal
          (string) @pattern))]]
  )

  vim.iter(query:iter_matches(root, buf)):each(function(_, node, _)
    local pattern_node = node[2][1]
    local text = vim.treesitter.get_node_text(pattern_node, buf)
    local start_row, start_col, end_row, _ = pattern_node:range()

    require 'monkey-patches.string' -- XXX string:find_all
    vim.iter(text:find_all(pattern)):each(function(interval)
      local spec_start, spec_end = unpack(interval)
      local buf_start = start_col + spec_start - 1
      local buf_end = start_col + spec_end
      vim.hl.range(
        buf,
        ns,
        'FormatSpecifier',
        { start_row, buf_start },
        { end_row, buf_end }
      )
    end)
  end)
end

vim.api.nvim_create_autocmd(
  { 'BufEnter', 'TextChanged', 'TextChangedI', 'InsertLeave' },
  {
    buffer = buf,
    callback = highlight_format_specifiers,
    group = vim.api.nvim_create_augroup('haskell-printf-specifiers', {}),
  }
)
