local nvim_treesitter_configs = require("nvim-treesitter.configs")

local M = {}

function M.setup()
  nvim_treesitter_configs.setup({
    highlight = {
      enable = true,
    },
    incremental_selection = {
      enable = true,
    },
    indent = {
      enable = true,
    },
  })
  vim.g.foldmethod = "expr"
  vim.g.foldexpr = "nvim_treesitter#foldexpr()"
end

return M
