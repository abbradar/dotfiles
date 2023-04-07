pcall(require, "impatient")

-- Indicate first time installation
local packer_bootstrap = false

-- Check if packer.nvim is installed
local fn = vim.fn
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system {
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  }
  vim.cmd [[packadd packer.nvim]]
end

local packer = require "packer"
packer.init({
  display = {
    open_fn = function()
      return require("packer.util").float {border = "single"}
    end,
    prompt_border = "single"
  },
})

packer.startup(function(use)
  use "wbthomason/packer.nvim"

  require("plugins").plugins(use)

  -- Bootstrap Neovim
  if packer_bootstrap then
    print "Restart Neovim required after installation!"
    packer.sync()
  end
end)

require("config").setup()
