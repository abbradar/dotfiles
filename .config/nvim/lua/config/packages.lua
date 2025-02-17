return {
  -- Autocompletion
  {
    "ms-jpq/coq_nvim",
    branch = "coq",
    event = "BufRead",
    config = function()
      require("config.plugins.coq").setup()
    end,
    dependencies = {
      { "ms-jpq/coq.artifacts", branch = "artifacts" },
      { "ms-jpq/coq.thirdparty", branch = "3p" },
    },
    build = ":COQdeps",
  },

  -- Syntax trees support
  {
    "nvim-treesitter/nvim-treesitter",
    event = "BufRead",
    config = function()
      require("config.plugins.nvim-treesitter").setup()
    end,
  },

  -- Comments
  {
    "terrortylor/nvim-comment",
    cmd = "CommentToggle",
    config = function()
      require("config.plugins.nvim-comment").setup()
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
    "phaazon/hop.nvim",
    config = function()
      require("config.plugins.hop").setup()
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
    "TimUntersberger/neogit",
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

  -- Smart line numbers
  "jeffkreeftmeijer/vim-numbertoggle",

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
    dependencies = { "ray-x/lsp_signature.nvim" },
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
  "tpope/vim-surround",
}
