local whichkey = require("which-key")

local M = {}

function M.setup()
  -- Leader
  vim.api.nvim_set_keymap("n", "<Space>", "<Nop>", { noremap = true, silent = true })
  vim.g.mapleader = " "

  --
  -- hop
  --
  whichkey.add({
    {
      "f",
      function()
        require("hop").hint_char1({
          direction = require("hop.hint").HintDirection.AFTER_CURSOR,
          current_line_only = true,
        })
      end,
      desc = "Hop forward to char",
      mode = "n",
    },
    {
      "F",
      function()
        require("hop").hint_char1({
          direction = require("hop.hint").HintDirection.BEFORE_CURSOR,
          current_line_only = true,
        })
      end,
      desc = "Hop backward to char",
      mode = "n",
    },
    {
      "f",
      function()
        require("hop").hint_char1({
          direction = require("hop.hint").HintDirection.AFTER_CURSOR,
          current_line_only = true,
          inclusive_jump = true,
        })
      end,
      desc = "Hop forward to char",
      mode = "o",
    },
    {
      "F",
      function()
        require("hop").hint_char1({
          direction = require("hop.hint").HintDirection.BEFORE_CURSOR,
          current_line_only = true,
          inclusive_jump = true,
        })
      end,
      desc = "Hop backward to char",
      mode = "o",
    },
    {
      "t",
      function()
        require("hop").hint_char1({
          direction = require("hop.hint").HintDirection.AFTER_CURSOR,
          current_line_only = true,
        })
      end,
      desc = "Hop forward until char",
    },
    {
      "T",
      function()
        require("hop").hint_char1({
          direction = require("hop.hint").HintDirection.BEFORE_CURSOR,
          current_line_only = true,
        })
      end,
      desc = "Hop backward until char",
    },
  })

  whichkey.add({
    {
      "<leader>w",
      function()
        require("hop").hint_words()
      end,
      desc = "Hint words",
      mode = { "n", "v" },
      remap = false,
    },
    {
      "<leader>e",
      function()
        require("hop").hint_words({ hint_position = require("hop.hint").HintPosition.END })
      end,
      desc = "Hint words",
      mode = { "n", "v" },
      remap = false,
    },
    {
      "<leader>w",
      function()
        require("hop").hint_words({ inclusive_jump = true })
      end,
      desc = "Hint words",
      mode = "o",
      remap = false,
    },
    {
      "<leader>e",
      function()
        require("hop").hint_words({
          hint_position = require("hop.hint").HintPosition.END,
          inclusive_jump = true,
        })
      end,
      desc = "Hint words",
      mode = "o",
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
