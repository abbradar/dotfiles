local hop = require "hop"
local set_keymap = vim.api.nvim_set_keymap

local M = {}

function M.setup()
  hop.setup({
    keys = "etovxqpdygfblzhckisuran"
  })
end

return M

