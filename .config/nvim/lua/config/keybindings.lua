local whichkey = require("which-key")

local M = {}

function M.setup()
  -- Leader
  vim.keymap.set("n", "<Space>", "<Nop>", { silent = true })
  vim.g.mapleader = " "

  --
  -- flash
  --
  whichkey.add({
    {
      "s",
      function()
        require("flash").jump()
      end,
      desc = "Flash jump",
      mode = { "n", "x", "o" },
    },
    {
      "S",
      function()
        require("flash").treesitter()
      end,
      desc = "Flash treesitter",
      mode = { "n", "x", "o" },
    },
    {
      "<leader>w",
      function()
        require("flash").jump({ pattern = [[\<\w]] })
      end,
      desc = "Jump to word start",
      mode = { "n", "x", "o" },
      remap = false,
    },
    {
      "<leader>e",
      function()
        require("flash").jump({ pattern = [[\w\>]], jump = { pos = "end" } })
      end,
      desc = "Jump to word end",
      mode = { "n", "x", "o" },
      remap = false,
    },
  })

  --
  -- Telescope
  --
  whichkey.add({
    {
      "<leader>`",
      function()
        require("telescope.builtin").find_files()
      end,
      desc = "Find file",
      remap = false,
    },
    {
      "<leader>.",
      function()
        require("telescope").extensions.file_browser.file_browser()
      end,
      desc = "Browse files",
      remap = false,
    },
    {
      "<leader>,",
      function()
        require("telescope.builtin").buffers({ sort_lastused = true, ignore_current_buffer = true })
      end,
      desc = "Switch buffers",
      remap = false,
    },
    {
      "<leader>/",
      function()
        require("telescope.builtin").live_grep()
      end,
      desc = "Search a word",
      remap = false,
    },
    {
      "<leader>:",
      function()
        require("telescope.builtin").command_history()
      end,
      desc = "Command history",
      remap = false,
    },
  })
end

function M.gui_setup()
  whichkey.add({
    {
      "<C-+>",
      function()
        require("config.gui").adjust_font_size(1)
      end,
      desc = "Increase font size",
      mode = { "n", "i" },
    },
    {
      "<C-->",
      function()
        require("config.gui").adjust_font_size(-1)
      end,
      desc = "Decrease font size",
      mode = { "n", "i" },
    },
  })
end

return M
