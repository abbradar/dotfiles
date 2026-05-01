local M = {}

function M.setup()
  vim.api.nvim_create_autocmd("FileType", {
    callback = function(args)
      local bufnr = args.buf
      if pcall(vim.treesitter.start, bufnr) then
        vim.bo[bufnr].indentexpr = 'v:lua.require("nvim-treesitter").indentexpr()'
      end
    end,
  })

  vim.opt.foldmethod = "expr"
  vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
end

return M
