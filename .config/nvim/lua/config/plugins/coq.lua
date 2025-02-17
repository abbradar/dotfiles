local coq = require("coq")

local M = {}

function M.setup()
  coq.Now("--shut-up")
end

return M
