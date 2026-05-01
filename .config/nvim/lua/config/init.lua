local keybindings = require("config.keybindings")

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

  local numbertoggle = vim.api.nvim_create_augroup("numbertoggle", { clear = true })
  vim.api.nvim_create_autocmd({ "BufEnter", "FocusGained", "InsertLeave", "CmdlineLeave", "WinEnter" }, {
    group = numbertoggle,
    callback = function()
      if vim.wo.number and vim.api.nvim_get_mode().mode ~= "i" then
        vim.wo.relativenumber = true
      end
    end,
  })
  vim.api.nvim_create_autocmd({ "BufLeave", "FocusLost", "InsertEnter", "CmdlineEnter", "WinLeave" }, {
    group = numbertoggle,
    callback = function()
      if vim.wo.number then
        vim.wo.relativenumber = false
      end
    end,
  })

  -- Autodetect tabs/spaces.
  vim.cmd([[autocmd BufRead * Sleuth]])

  -- Make directory follow the file.
  vim.opt.autochdir = true

  -- Bash-like completion.
  vim.opt.wildmode = "longest:full,full"

  -- Use system clipboard.
  vim.opt.clipboard = "unnamedplus"

  keybindings.setup()
end

return M
