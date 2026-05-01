local servers = {
  ccls = {},
  hls = {},
  fsautocomplete = {},
}

local M = {}

function M.setup()
  vim.lsp.config("*", {
    capabilities = require("blink.cmp").get_lsp_capabilities(),
  })

  for server_name, server_opts in pairs(servers) do
    vim.lsp.config(server_name, { init_options = server_opts })
    vim.lsp.enable(server_name)
  end
end

return M
