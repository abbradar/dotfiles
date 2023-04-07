local whichkey = require "which-key"

local M = {}

local default_opts = { noremap = true, silent = true }

local function keymap(mode, key, cmd)
  vim.api.nvim_set_keymap(mode, key, cmd, default_opts)
end

function M.setup()
  -- Leader
  keymap('n', '<Space>', "<Nop>")
  vim.g.mapleader = " "

  --
  -- hop
  --
  keymap('n', 'f', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<CR>")
  keymap('n', 'F', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<CR>")
  keymap('o', 'f', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true, inclusive_jump = true })<CR>")
  keymap('o', 'F', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true, inclusive_jump = true })<CR>")
  keymap('', 't', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<CR>")
  keymap('', 'T', "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<CR>")

  whichkey.register({
    ["w"] = { "<cmd>lua require'hop'.hint_words()<CR>", "Hint words", noremap = true },
    ["e"] = { "<cmd>lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.END })<CR>", "Hint words", noremap = true },
  }, { prefix = "<leader>", mode = "n" })
  whichkey.register({
    ["w"] = { "<cmd>lua require'hop'.hint_words()<CR>", "Hint words", noremap = true },
    ["e"] = { "<cmd>lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.END })<CR>", "Hint words", noremap = true },
  }, { prefix = "<leader>", mode = "v" })
  whichkey.register({
    ["w"] = { "<cmd>lua require'hop'.hint_words({ inclusive_jump = true })<CR>", "Hint words", noremap = true },
    ["e"] = { "<cmd>lua require'hop'.hint_words({ hint_position = require'hop.hint'.HintPosition.END, inclusive_jump = true })<CR>", "Hint words", noremap = true },
  }, { prefix = "<leader>", mode = "o" })

  --
  -- Telescope
  --
  whichkey.register({
    ["`"] = { "<cmd>lua require'telescope.builtin'.find_files()<CR>", "Find file", noremap = true },
    ["."] = { "<cmd>lua require'telescope'.extensions.file_browser.file_browser()<CR>", "Browse files", noremap = true },
    [","] = { "<cmd>lua require'telescope.builtin'.buffers({ sort_lastused = true, ignore_current_buffer = true })<CR>", "Switch buffers", noremap = true },
    ["/"] = { "<cmd>lua require'telescope.builtin'.live_grep()<CR>", "Search a word", noremap = true },
    [":"] = { "<cmd>lua require'telescope.builtin'.command_history()<CR>", "Command history", noremap = true },
  }, { prefix = "<leader>" })
end

function M.gui_setup()
  keymap('', '<C-+>', "<cmd>lua require('config.gui').adjust_font_size(1)<CR>")
  keymap('i', '<C-+>', "<cmd>lua require('config.gui').adjust_font_size(1)<CR>")
  keymap('', '<C-->', "<cmd>lua require('config.gui').adjust_font_size(-1)<CR>")
  keymap('i', '<C-->', "<cmd>lua require('config.gui').adjust_font_size(-1)<CR>")
end

return M
