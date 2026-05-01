return {
  -- Autocompletion
  {
    "saghen/blink.cmp",
    version = "*",
    event = "InsertEnter",
    config = function()
      require("config.plugins.blink").setup()
    end,
  },

  -- Syntax trees support
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    build = ":TSUpdate",
    event = "BufRead",
    config = function()
      require("config.plugins.nvim-treesitter").setup()
    end,
  },

  -- Hotkey completion
  {
    "folke/which-key.nvim",
    config = function()
      require("config.plugins.which-key").setup()
    end,
  },

  -- Motion
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    config = function()
      require("config.plugins.flash").setup()
    end,
  },

  -- Autopairs
  {
    "windwp/nvim-autopairs",
    event = "BufRead",
    config = function()
      require("config.plugins.nvim-autopairs").setup()
    end,
  },

  -- File explorer
  {
    "nvim-tree/nvim-tree.lua",
    cmd = "NvimTreeToggle",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("config.plugins.nvim-tree").setup()
    end,
  },

  -- Git integration
  {
    "lewis6991/gitsigns.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    event = "BufRead",
    config = function()
      require("config.plugins.gitsigns").setup()
    end,
  },
  {
    "NeogitOrg/neogit",
    cmd = "Neogit",
    config = function()
      require("config.plugins.neogit").setup()
    end,
  },

  -- F#
  {
    "ionide/Ionide-vim",
    ft = "fsharp",
    config = function()
      require("config.plugins.ionide").setup()
    end,
  },

  -- Fuzzy search
  {
    "nvim-telescope/telescope.nvim",
    cmd = "Telescope",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-file-browser.nvim",
    },
    config = function()
      require("config.plugins.telescope").setup()
    end,
  },

  -- LSP Configuration
  {
    "neovim/nvim-lspconfig",
    event = "BufReadPre",
    config = function()
      require("config.plugins.lsp").setup()
    end,
  },

  -- disable mason.nvim, use programs.neovim.extraPackages
  { "williamboman/mason.nvim", enabled = false },
  { "williamboman/mason-lspconfig.nvim", enabled = false },

  -- Color themes
  {
    "echasnovski/mini.nvim",
    version = "*",
  },

  -- Tabs
  {
    "akinsho/bufferline.nvim",
    version = "*",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("config.plugins.bufferline").setup()
    end,
  },

  -- Status line
  {
    "nvim-lualine/lualine.nvim",
    lazy = false,
    priority = 1000,
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("config.plugins.lualine").setup()
    end,
  },

  -- Quickfix panel
  {
    "folke/trouble.nvim",
    cmd = { "Trouble", "TroubleClose", "TroubleRefresh", "TroubleToggle" },
    dependencies = "nvim-tree/nvim-web-devicons",
    config = function()
      require("config.plugins.trouble").setup()
    end,
  },

  -- Autodetect tabs
  {
    "tpope/vim-sleuth",
    cmd = "Sleuth",
  },

  -- Automatic surroundings
  "tpope/vim-repeat",
  {
    "kylechui/nvim-surround",
    version = "*",
    event = "BufRead",
    opts = {},
  },
}
