local api = vim.api

local indent_level = {
  title = 0,
  chapter = 0,
  section = 0,
  subsection = 1,
  subsubsection = 2,
  paragraph = 3,
  subparagraph = 4,
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
  if parser:lang() ~= 'latex' then
    vim.notify_once(('Bad parser: %s, expect latex'):format(parser:lang()), vim.log.levels.WARN)
    return
  end

  local query = vim.treesitter.query.parse(
    'latex',
    [[(title_declaration) @title
    (chapter) @chapter
    (section) @section
    (subsection) @subsection
    (subsubsection) @subsubsection
    (paragraph) @paragraph
    (subparagraph) @subparagraph]]
  )

  local root = parser:parse()[1]:root()
  local toc = vim
    .iter(query:iter_captures(root, bufnr))
    :map(function(id, node, _, _)
      local type = query.captures[id]
      local lnum, col, end_lnum, end_col = node:range()
      local header = vim.trim(
        vim.treesitter.get_node_text(node:field('text')[1], bufnr):match('{(.*)}'):gsub('%s+', ' ')
      )
      return {
        bufnr = bufnr,
        lnum = lnum + 1,
        end_lnum = end_lnum + 1,
        col = col + 1,
        end_col = end_col, -- exclusive
        text = ('  '):rep(indent_level[type]) .. (type == 'title' and header:upper() or header),
      }
    end)
    :totable()

  vim.fn.setloclist(0, toc, ' ')
  vim.fn.setloclist(0, {}, 'a', { title = 'LaTeX TOC' })
  vim.cmd.lopen()
  vim.w.qf_toc = bufname
end)
