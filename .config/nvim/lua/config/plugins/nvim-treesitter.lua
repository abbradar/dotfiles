local nvim_treesitter_install = require("nvim-treesitter.install")
local nvim_treesitter_configs = require("nvim-treesitter.configs")

local M = {}

function M.setup()
  -- Hack to allow compiling C++.
  nvim_treesitter_install.compilers = {"clang++"}
  nvim_treesitter_configs.setup {
    ensure_installed = {
      "c",
      "cpp",
      "glsl",
      "elixir",
      "python",
    },
    highlight = {
      enable = true
    },
    incremental_selection = {
      enable = true
    },
    indent = {
      enable = true
    },
  }
  vim.g.foldmethod = "expr"
  vim.g.foldexpr = "nvim_treesitter#foldexpr()"
end

return M

