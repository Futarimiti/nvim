local api = vim.api

local indent_level = {
  h1_content = 0,
  h2_content = 0,
  h3_content = 1,
  h4_content = 2,
  h5_content = 3,
  h6_content = 4,
}

vim.keymap.set('n', 'gO', function()
  local bufnr = api.nvim_get_current_buf()
  local bufname = api.nvim_buf_get_name(bufnr)
  local info = vim.fn.getloclist(0, { winid = 1 })
  if info ~= '' and vim.w[info.winid].qf_toc == bufname then
    vim.cmd.lopen()
    return
  end
  local parser = vim.treesitter.get_parser(bufnr)
  if parser:lang() ~= 'markdown' then
    vim.notify_once(('Bad parser: %s, expect markdown'):format(parser:lang()), vim.log.levels.WARN)
    return
  end

  local query = vim.treesitter.query.parse(
    'markdown',
    [[
      (section (setext_heading heading_content: (_) (setext_h1_underline)) @h1_content)
      (section (setext_heading heading_content: (_) (setext_h2_underline)) @h2_content)

      (section (atx_heading (atx_h1_marker) heading_content: (_)) @h1_content)
      (section (atx_heading (atx_h2_marker) heading_content: (_)) @h2_content)
      (section (atx_heading (atx_h3_marker) heading_content: (_)) @h3_content)
      (section (atx_heading (atx_h4_marker) heading_content: (_)) @h4_content)
      (section (atx_heading (atx_h5_marker) heading_content: (_)) @h5_content)
      (section (atx_heading (atx_h6_marker) heading_content: (_)) @h6_content)
    ]]
  )

  local root = parser:parse()[1]:root()
  local toc = vim
    .iter(query:iter_captures(root, bufnr))
    :map(function(id, node, _, _)
      local type = query.captures[id]
      local lnum, col, end_lnum, end_col = node:range()
      local header = vim.trim(vim.treesitter.get_node_text(node:field('heading_content')[1], bufnr))
      return {
        bufnr = bufnr,
        lnum = lnum + 1,
        end_lnum = end_lnum + 1,
        col = col + 1,
        end_col = end_col, -- exclusive
        text = ('  '):rep(indent_level[type])
          .. (type == 'h1_content' and header:upper() or header),
      }
    end)
    :totable()

  vim.fn.setloclist(0, toc, ' ')
  vim.fn.setloclist(0, {}, 'a', { title = 'Markdown TOC' })
  vim.cmd.lopen()
  vim.w.qf_toc = bufname
end)
