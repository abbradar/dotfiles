local keybindings = require "config.keybindings"

local M = {}

function M.setup()
  -- Disable unneeded plugins.
  vim.g.loaded_gzip = false
  vim.g.loaded_tarPlugin = false
  vim.g.loaded_zipPlugin = false
  vim.g.loaded_2html_plugin = false

  -- UI.
  vim.opt.mouse = "a"
  vim.opt.termguicolors = true

  -- Line numbers.
  vim.opt.cursorline = true
  vim.opt.number = true
  vim.opt.relativenumber = true

  -- Autodetect tabs/spaces.
  vim.cmd [[autocmd BufRead * silent Sleuth]]

  -- Make directory follow the file.
  vim.opt.autochdir = true

  -- Bash-like completion.
  vim.opt.wildmode = "longest:full,full"

  -- Use system clipboard.
  vim.opt.clipboard = "unnamedplus"

  keybindings.setup()
end

return M
