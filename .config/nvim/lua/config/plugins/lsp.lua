local lspconfig = require("lspconfig")
local coq = require("coq")
local lsp_signature = require("lsp_signature")

local servers = {
  ccls = {},
  hls = {},
  fsautocomplete = {},
}

local M = {}

local function on_attach(client, bufnr)
  lsp_signature.on_attach(client, bufnr)

  -- Enable completion triggered by <C-X><C-O>
  -- See `:help omnifunc` and `:help ins-completion` for more information.
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  -- Use LSP as the handler for formatexpr.
  -- See `:help formatexpr` for more information.
  vim.api.nvim_buf_set_option(0, "formatexpr", "v:lua.vim.lsp.formatexpr()")
end

function M.setup()
  for server_name, server_opts in pairs(servers) do
    lspconfig[server_name].setup(coq.lsp_ensure_capabilities({
      init_options = server_opts,
      on_attach = on_attach,
    }))
  end
end

return M
