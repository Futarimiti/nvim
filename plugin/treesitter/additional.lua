local parser_config = require('nvim-treesitter.parsers').get_parser_configs()
-- NOTE SLOW AF
parser_config.lean = {
  install_info = {
    url = 'https://github.com/Julian/tree-sitter-lean.git',
    files = { 'src/parser.c', 'src/scanner.cc' },
  },
}
