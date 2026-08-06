-- Neovim configuration file

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
vim.g.transparent_enabled = true

local opt = vim.opt
local cmd = vim.cmd

opt.clipboard = "unnamedplus"
opt.cursorline = true
opt.expandtab = true
opt.hlsearch = false
opt.ignorecase = true
opt.incsearch = true
opt.number = true
opt.relativenumber = true
opt.laststatus = 2
opt.scrolloff = 8
opt.shiftwidth = 4
opt.signcolumn = "no"
opt.smartcase = true
opt.smartindent = true
opt.tabstop = 4
opt.termguicolors = true
opt.undofile = true
opt.updatetime = 300
opt.wrap = false

cmd("set noshowmode")
cmd("set noshowcmd")
cmd("set noruler")

vim.pack.add({
    "https://github.com/NeogitOrg/neogit",
    "https://github.com/akinsho/toggleterm.nvim.git",
    "https://github.com/aserowy/tmux.nvim",
    "https://github.com/brenton-leighton/multiple-cursors.nvim",
    "https://github.com/hrsh7th/cmp-buffer",
    "https://github.com/hrsh7th/cmp-cmdline",
    "https://github.com/hrsh7th/cmp-nvim-lsp",
    "https://github.com/hrsh7th/cmp-path",
    "https://github.com/hrsh7th/nvim-cmp",
    "https://github.com/neovim/nvim-lspconfig",
    "https://github.com/nvim-lua/plenary.nvim",
    "https://github.com/nvim-telescope/telescope.nvim",
    "https://github.com/smoka7/hop.nvim",
    "https://github.com/stevearc/oil.nvim",
    "https://github.com/xiyaowong/transparent.nvim",
    "https://github.com/silentium-theme/silentium.nvim.git",
})

require("toggleterm").setup({
    open_mapping = [[<c-t>]]
})

require("oil").setup({
    watch_for_changes = true,
    columns = { "icon", "permissions", "size", "mtime" },
    view_options = { show_hidden = true },
    delete_to_trash = true,
    skip_confirm_for_simple_edits = true,
    keymaps = {
        ["<C-h>"] = false,
        ["<C-l>"] = false,
        ["q"] = { "actions.close", mode = "n" }
    },
})

require("hop").setup({})
require("multiple-cursors").setup({})
require("tmux").setup({})

vim.diagnostic.config({
    virtual_text = false,
    underline = false
})

vim.lsp.enable("bashls")
vim.lsp.enable("clangd")
vim.lsp.enable("gopls")
vim.lsp.enable("lua_ls")
vim.lsp.enable("pyright")

local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }
local telescope_builtin = require("telescope.builtin")

keymap("n", "<leader>bd", ":bd<CR>", opts)
keymap("n", "<leader>e", ":Oil<CR>", opts)
keymap("n", "<leader>g", ":Neogit<CR>", opts)
keymap("n", "<leader>y", '"+y', opts)
keymap("n", "<leader>t", ":TermToggle<CR>", opts)
keymap("n", "<leader>r", ":lua vim.lsp.buf.rename()<CR>", opts)

keymap({ "n", "x" }, "gz", "<Cmd>MultipleCursorsAddMatches<CR>", opts)
keymap({ "n", "x" }, "<C-n>", "<Cmd>MultipleCursorsAddJumpNextMatch<CR>", opts)
keymap("", "f", function() require("hop").hint_char1({ current_line_only = false }) end, opts)

keymap("n", "<leader>ds", telescope_builtin.diagnostics, opts)
keymap("n", "<leader>fb", telescope_builtin.buffers, opts)
keymap("n", "<leader>ff", telescope_builtin.find_files, opts)
keymap("n", "<leader>fg", telescope_builtin.live_grep, opts)
keymap("n", "<leader>fh", telescope_builtin.help_tags, opts)
keymap("n", "<leader>fm", function() telescope_builtin.man_pages({ sections = { "ALL" } }) end, opts)
keymap("n", "<leader>fr", telescope_builtin.oldfiles, opts)

keymap("t", "<Esc><Esc>", [[<C-\><C-n>]], opts)
keymap("t", "<C-h>", [[<C-\><C-n><C-w>h]], opts)
keymap("t", "<C-j>", [[<C-\><C-n><C-w>j]], opts)
keymap("t", "<C-k>", [[<C-\><C-n><C-w>k]], opts)
keymap("t", "<C-l>", [[<C-\><C-n><C-w>l]], opts)
keymap("t", "<A-h>", [[<C-\><C-n><C-w><]], opts)
keymap("t", "<A-l>", [[<C-\><C-n><C-w>>]], opts)
keymap("t", "<A-j>", [[<C-\><C-n><C-w>-]], opts)
keymap("t", "<A-k>", [[<C-\><C-n><C-w>+]], opts)
keymap("t", "<C-f>", [[<C-\><C-n><C-f>]], opts)
keymap("t", "<C-b>", [[<C-\><C-n><C-b>]], opts)

vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*",
    command = [[lua vim.lsp.buf.format()]],
})

vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*",
    command = [[%s/\s\+$//e]],
})

cmd("colorscheme silentium")
