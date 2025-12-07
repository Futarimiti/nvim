vim.cmd.packadd 'stringbreaker.nvim'

local sb = require 'string-breaker'

sb.setup()
vim.api.nvim_create_user_command('BS', sb.break_string, {})
