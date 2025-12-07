-- format!-string escape highlights
-- TODO very crappy - tidy up sometime
-- TODO support write! and writeln!
local ns = vim.api.nvim_create_namespace 'rust-format-escapes'

vim.api.nvim_set_hl(0, 'FormatInterpolateBraces', { link = 'Special' })
vim.api.nvim_set_hl(0, 'FormatInterpolateContent', { link = 'Identifier' })
vim.api.nvim_set_hl(0, 'FormatDoubleBraces', { link = 'Special' })

local buf = vim.api.nvim_get_current_buf()

local highlight_format_specials = function()
  vim.api.nvim_buf_clear_namespace(buf, ns, 0, -1)

  local parser = vim.treesitter.get_parser(buf, 'rust')
  local tree = parser:parse()[1]
  local root = tree:root()

  -- Enhanced query to capture all format strings
  local query = vim.treesitter.query.parse(
    'rust',
    [[(macro_invocation
        macro: (identifier) @macro
        (#any-of? @macro "format" "print" "eprint" "println" "eprintln")
        (token_tree
          (string_literal
            (string_content) @string)))]]
  )

  vim.iter(query:iter_matches(root, buf)):each(function(_, node, _)
    local string_node = node[2][1]
    local text = vim.treesitter.get_node_text(string_node, buf)
    -- NOTE strangely, "\n" makes two nodes, shouldn't matter though
    local start_row, start_col, end_row, _ = string_node:range()

    -- First pass: Highlight escaped braces
    local escaped_regions = {}
    local offset = 1
    while true do
      local esc_start, esc_end, what = text:find('([{}][{}])', offset)
      if not esc_start then break end

      if what == '{{' or what == '}}' then
        -- Calculate positions and highlight
        local buf_start = start_col + esc_start - 1
        local buf_end = start_col + 1 + esc_end - 1

        vim.hl.range(
          buf,
          ns,
          'FormatDoubleBraces',
          { start_row, buf_start },
          { end_row, buf_end }
        )

        -- Record escaped regions to avoid overlap
        table.insert(escaped_regions, { esc_start, esc_end })
        offset = esc_end + 1
      else
        offset = esc_start + 1
      end
    end

    -- Second pass: Highlight variables (excluding escaped regions)
    offset = 1
    while true do
      local interpolate_start, interpolate_end = text:find('{(.-)}', offset)
      if not interpolate_start then break end
      if text:sub(interpolate_start + 1, interpolate_start + 1) == '{' then
        break
      end

      -- Check if within any escaped region
      local in_escaped = false
      for _, region in ipairs(escaped_regions) do
        if interpolate_start >= region[1] and interpolate_end <= region[2] then
          in_escaped = true
          break
        end
      end

      if not in_escaped then
        local buf_start = start_col + interpolate_start - 1
        local buf_end = start_col + 1 + interpolate_end - 1

        vim.hl.range(
          buf,
          ns,
          'FormatInterpolateBraces',
          { start_row, buf_start },
          { end_row, buf_end }
        )

        local var_start = buf_start + 1
        local var_end = buf_end - 1
        if var_start < var_end then
          vim.hl.range(
            buf,
            ns,
            'FormatInterpolateContent',
            { start_row, var_start },
            { end_row, var_end }
          )
        end
      end

      offset = interpolate_end + 1
    end
  end)
end

-- Refresh highlights on changes
vim.api.nvim_create_autocmd(
  { 'BufEnter', 'TextChanged', 'TextChangedI', 'InsertLeave' },
  {
    buffer = buf,
    callback = highlight_format_specials,
    group = vim.api.nvim_create_augroup('rust-format-string-interpolate', {}),
  }
)
