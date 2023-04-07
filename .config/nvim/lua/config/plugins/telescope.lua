local telescope = require("telescope")

local M = {}

function M.setup()
  telescope.setup()
  telescope.load_extension("file_browser")
end

return M
