local keybindings = require("config.keybindings")

local M = {}

local font_name = "Source Code Pro"
local font_size = 12

function M.adjust_font_size(amount)
  local new_font_size = font_size + amount
  if new_font_size > 0 then
    vim.cmd("GuiFont! " .. font_name .. ":h" .. new_font_size)
    font_size = new_font_size
  end
end

function M.setup()
  -- Disable GUI tabs.
  vim.cmd([[GuiTabline 0]])

  keybindings.gui_setup()
end

return M
