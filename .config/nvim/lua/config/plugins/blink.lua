local blink = require("blink.cmp")

local M = {}

function M.setup()
  blink.setup({
    keymap = { preset = "default" },
    signature = { enabled = true },
    sources = {
      default = { "lsp", "path", "snippets", "buffer" },
    },
  })
end

return M
