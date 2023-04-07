local M = {}

function M.plugins(use)
  -- Autocompletion.
  use {
    "ms-jpq/coq_nvim",
    branch = "coq",
    event = "BufRead",
    module = "coq",
    config = function() require("config.plugins.coq").setup() end,
    requires = {
      { "ms-jpq/coq.artifacts", branch = "artifacts" },
      { "ms-jpq/coq.thirdparty", branch = "3p", module = "coq_3p" },
    },
    run = ":COQdeps",
  }

  -- Comments.
  use {
    "terrortylor/nvim-comment",
    cmd = "CommentToggle",
    config = function() require("config.plugins.nvim-comment").setup() end,
  }

  -- Hotkey completion.
  use {
    "folke/which-key.nvim",
    config = function() require("config.plugins.which-key").setup() end,
  }

  -- Motion.
  use {
    "phaazon/hop.nvim",
    module = "hop",
    config = function() require("config.plugins.hop").setup() end,
  }

  -- Syntax trees support.
  -- TODO: disabled for now, too unstable.
  --[[
  use {
    "nvim-treesitter/nvim-treesitter",
    event = "BufRead",
    config = function() require("config.plugins.nvim-treesitter").setup() end,
    run = ":TSUpdate",
  }
  --]]

  -- Autopairs.
  use {
    "windwp/nvim-autopairs",
    event = "BufRead",
    config = function() require("config.plugins.nvim-autopairs").setup() end,
  }

  -- File explorer.
  use {
    "kyazdani42/nvim-tree.lua",
    cmd = "NvimTreeToggle",
    requires = {"kyazdani42/nvim-web-devicons"},
    config = function() require("config.plugins.nvim-tree").setup() end,
  }

  -- Git.
  use {
    "lewis6991/gitsigns.nvim",
    requires = {"nvim-lua/plenary.nvim"},
    event = "BufRead",
    config = function() require("config.plugins.gitsigns").setup() end,
  }
  use {
    "TimUntersberger/neogit",
    cmd = "Neogit",
    module = "neogit",
    config = function() require("config.plugins.neogit").setup() end,
  }

  -- F#
  use {
    "ionide/Ionide-vim",
    ft = {"fsharp"},
    config = function() require("config.plugins.ionide").setup() end,
  }

  -- Smart line numbers.
  use {
    "jeffkreeftmeijer/vim-numbertoggle",
  }

  -- Fuzzy search.
  use {
    "nvim-telescope/telescope.nvim",
    cmd = "Telescope",
    module = {"telescope", "telescope.builtin"},
    requires = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-file-browser.nvim",
    },
    config = function() require("config.plugins.telescope").setup() end,
  }

  -- LSP.
  use {
    "neovim/nvim-lspconfig",
    event = "BufReadPre",
    module = { "lspconfig", "lspconfig.util" },
    config = function() require("config.plugins.lsp").setup() end,
    requires = {"ray-x/lsp_signature.nvim"},
  }

  use {
    "williamboman/mason.nvim",
    module = "mason",
  }

  use {
    "williamboman/mason-lspconfig.nvim",
    module = "mason-lspconfig",
  }

  -- Color theme from Doom Emacs.
  use {
    "NTBBloodbath/doom-one.nvim",
    config = function() require("config.plugins.doom-one").setup() end,
  }

  -- Tabs.
  use {
    "akinsho/bufferline.nvim",
    tag = "v2.*",
    requires = {"kyazdani42/nvim-web-devicons"},
    config = function() require("config.plugins.bufferline").setup() end,
  }

  -- Status line.
  use {
    "NTBBloodbath/galaxyline.nvim",
    requires = {"kyazdani42/nvim-web-devicons"},
    config = function() require("config.plugins.galaxyline").setup() end,
  }

  -- Quickfix panel.
  use {
    "folke/trouble.nvim",
    cmd = { "Trouble", "TroubleClose", "TroubleRefresh", "TroubleToggle" },
    requires = "kyazdani42/nvim-web-devicons",
    config = function() require("config.plugins.trouble").setup() end,
  }

  -- Autodetect tabs.
  use {
    "tpope/vim-sleuth",
    cmd = "Sleuth",
  }

  -- Performance.
  use "lewis6991/impatient.nvim"
  use {
    "nathom/filetype.nvim",
    config = function() require("config.plugins.filetype").setup() end,
  }

  -- Automatic surroundings.
  use "tpope/vim-repeat"
  use "tpope/vim-surround"
end

return M
