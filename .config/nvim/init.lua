-- Needs to be set before loading plugins
vim.g.mapleader = " "
require("lazy").setup({
  dev = {
    -- reuse files from pkgs.vimPlugins.*
    path = lazyPath,
    patterns = { "" },
    -- fallback to download
    fallback = true,
  },
  spec = {
    { import = "config.packages" },
  },
})
require("config").setup()
