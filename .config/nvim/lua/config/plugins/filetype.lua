local filetype = require "filetype"

local M = {}

function M.setup()
  filetype.setup({
    overrides = {
      extensions = {
        ["fs"] = "fsharp",

        -- Shaders
        ["vert"] = "glsl",
        ["tesc"] = "glsl",
        ["tese"] = "glsl",
        ["geom"] = "glsl",
        ["frag"] = "glsl",
        ["comp"] = "glsl",
      },
    },
  })
end

return M

