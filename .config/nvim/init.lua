-- Neovim configuration file

local g = vim.g
local opt = vim.opt
local cmd = vim.cmd

g.mapleader = " "
g.maplocalleader = "\\"
opt.cursorline = true
opt.expandtab = true
opt.hlsearch = false
opt.ignorecase = true
opt.incsearch = true
opt.number = true
opt.relativenumber = true
opt.shiftwidth = 4
opt.tabstop = 4
opt.signcolumn = "no"
opt.smartcase = true
opt.smartindent = true
opt.undofile = false
cmd("set noshowcmd")
cmd("set noruler")
cmd("set noshowmode")
cmd("set shortmess+=I")

vim.pack.add({
    "https://github.com/NeogitOrg/neogit",
    "https://github.com/akinsho/toggleterm.nvim.git",
    "https://github.com/aserowy/tmux.nvim",
    "https://github.com/hrsh7th/cmp-buffer",
    "https://github.com/hrsh7th/cmp-cmdline",
    "https://github.com/hrsh7th/cmp-nvim-lsp",
    "https://github.com/hrsh7th/cmp-path",
    "https://github.com/hrsh7th/nvim-cmp",
    "https://github.com/neovim/nvim-lspconfig",
    "https://github.com/nvim-lua/plenary.nvim",
    "https://github.com/nvim-telescope/telescope-fzf-native.nvim",
    "https://github.com/nvim-telescope/telescope.nvim",
    "https://github.com/smithbm2316/centerpad.nvim.git",
    "https://github.com/smoka7/hop.nvim",
    "https://github.com/stevearc/oil.nvim",
    "https://github.com/xiyaowong/transparent.nvim",
})

local cmp = require("cmp")
require("cmp").setup({
    completion = { autocomplete = false },
    window = {},
    mapping = cmp.mapping.preset.insert({
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-Space>"] = cmp.mapping.complete(),
        ["<C-e>"] = cmp.mapping.abort(),
        ["<CR>"] = cmp.mapping.confirm({ select = true }),
    }),
    sources = cmp.config.sources({
        { name = "nvim_lsp" },
        { name = "path" },
        { name = "buffer" }
    })
})

require('telescope').setup({
    defaults = require("telescope.themes").get_dropdown({
        initial_mode = "insert",
        mappings = {
            i = { ["<Esc>"] = require("telescope.actions").close }
        }
    }),

    pickers = {
        live_grep = {
            additional_args = {
                "--hidden",
                "--glob",
                "!**/.git/*"
            }
        },
        find_files = {
            find_command = {
                "fd",
                "--type", "f",
                "--type", "d",
                "--hidden",
                "--follow",
                "--exclude", ".git"
            }
        }
    }
})

require("toggleterm").setup({
    open_mapping = [[<c-t>]],
    direction = "float"
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
require("tmux").setup({})

vim.g.transparent_enabled = true

vim.diagnostic.config({
    virtual_text = false,
    underline = true,
    float = {
        border = "rounded",
        source = "if_many",
        header = "",
        prefix = "",
    },
})

vim.lsp.enable("bashls")
vim.lsp.enable("clangd")
vim.lsp.enable("gopls")
vim.lsp.enable("lua_ls")
vim.lsp.enable("pyright")
vim.lsp.semantic_tokens.enable(false)

local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }
local builtin = require('telescope.builtin')

keymap("n", "<leader>e", ":Oil<CR>", opts)
keymap("n", "<leader>c", ":make<CR>", opts)
keymap("n", "<leader>g", ":Neogit<CR>", opts)
keymap("n", "<leader>t", ":TermToggle<CR>", opts)
keymap("n", "<leader>r", ":lua vim.lsp.buf.rename()<CR>", opts)
keymap("n", "<leader>dl", ":lua vim.diagnostic.setqflist()<CR>", opts)
keymap("n", "<leader>dt", ":lua vim.diagnostic.enable(not vim.diagnostic.is_enabled())<CR>", opts)
keymap("n", "<leader>df", ":lua vim.diagnostic.open_float()<CR>", opts)
keymap("v", "<leader>y", '"*y')
keymap("", "f", function() require("hop").hint_char1({ current_line_only = false }) end, opts)
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
keymap("n", "<leader>f", builtin.find_files, opts)
keymap("n", "<leader>F", function() builtin.find_files({ cwd = vim.fn.expand("%:p:h") }) end, opts)
keymap("n", "<leader>b", builtin.buffers, opts)
keymap("n", "<leader>m", function() builtin.man_pages({ sections = { "ALL" } }) end, opts)

vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*",
    command = [[lua vim.lsp.buf.format()]],
})

vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*",
    command = [[%s/\s\+$//e]],
})
