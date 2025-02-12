pcall(require, "impatient")

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
