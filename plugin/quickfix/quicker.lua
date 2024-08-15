local quicker = require 'quicker'

quicker.setup {
  borders = {
    vert = ' ',
    strong_cross = '━',
    soft_cross = '╌',
  },
  keys = {
    {
      'zR',
      quicker.expand,
      desc = 'Expand quickfix context',
    },
    {
      'zM',
      quicker.collapse,
      desc = 'Collapse quickfix context',
    },
  },
}
