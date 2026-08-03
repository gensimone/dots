-- Neovim Configuration File.

-----------------------------------------------------------
-- Core Options & Globals
-----------------------------------------------------------
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
vim.g.transparent_enabled = true

local opt = vim.opt
local cmd = vim.cmd
local diagnostic = vim.diagnostic.config

opt.clipboard = "unnamedplus"
opt.guicursor = "n-v-c-i:block-blinkwait1000-blinkon500-blinkoff500"
opt.cursorline = false
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
opt.fillchars = { eob = " " }

cmd("set noshowmode")
cmd("set noshowcmd")
cmd("set noruler")
cmd("set shortmess+=I")

diagnostic({ underline = false })

-----------------------------------------------------------
-- Plugins Management
-----------------------------------------------------------
vim.pack.add({
    "https://github.com/NeogitOrg/neogit",
    "https://github.com/aserowy/tmux.nvim",
    "https://github.com/brenton-leighton/multiple-cursors.nvim",
    "https://github.com/ej-shafran/compile-mode.nvim",
    "https://github.com/hrsh7th/cmp-buffer",
    "https://github.com/hrsh7th/cmp-cmdline",
    "https://github.com/hrsh7th/cmp-nvim-lsp",
    "https://github.com/hrsh7th/cmp-path",
    "https://github.com/hrsh7th/nvim-cmp",
    "https://github.com/neovim/nvim-lspconfig",
    "https://github.com/nvim-lua/plenary.nvim",
    "https://github.com/nvim-telescope/telescope.nvim",
    "https://github.com/nvim-tree/nvim-tree.lua",
    "https://github.com/smoka7/hop.nvim",
    "https://github.com/stevearc/oil.nvim",
    "https://github.com/xiyaowong/transparent.nvim",
    "https://github.com/akinsho/toggleterm.nvim.git",
})

-- Support for Java and Spring Boot (Commented out)
-- vim.pack.add({
--     {
--         src = 'https://github.com/JavaHello/spring-boot.nvim',
--         version = '218c0c26c14d99feca778e4d13f5ec3e8b1b60f0',
--     },
--     'https://github.com/MunifTanjim/nui.nvim',
--     'https://github.com/mfussenegger/nvim-dap',
--     'https://github.com/nvim-java/nvim-java',
-- })

-----------------------------------------------------------
-- Plugin Configurations
-----------------------------------------------------------

-- nvim-tree
require("nvim-tree").setup({
    sort = {
        sorter = "case_sensitive",
    },
    view = {
        width = 30,
    },
    renderer = {
        group_empty = true,
    },
    filters = {
        dotfiles = true,
    },
})

-- toggleterm
require("toggleterm").setup({
    size = function(term)
        if term.direction == "horizontal" then
            return 15
        elseif term.direction == "vertical" then
            return vim.o.columns * 0.4
        end
    end,
    open_mapping = [[<c-t>]],
    hide_numbers = true,
    shade_filetypes = {},
    autochdir = true,
    shade_terminals = true,
    start_in_insert = true,
    insert_mappings = true,
    terminal_mappings = true,
    persist_size = true,
    persist_mode = true,
    direction = 'tab',
    close_on_exit = true,
    clear_env = false,
    shell = vim.o.shell,
    auto_scroll = true,
})

-- nvim-cmp
local cmp = require("cmp")
cmp.setup({
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
    }),
})

-- oil.nvim
require("oil").setup({
    watch_for_changes = true,
    default_file_explorer = true,
    columns = { "permissions", "size", "mtime" },
    view_options = { show_hidden = true },
    delete_to_trash = false,
    skip_confirm_for_simple_edits = true,
    prompt_save_on_select_new_entry = true,
    keymaps = {
        ["g?"] = { "actions.show_help", mode = "n" },
        ["<CR>"] = "actions.select",
        ["<C-s>"] = { "actions.select", opts = { vertical = true } },
        ["<C-h>"] = false,
        ["<C-t>"] = { "actions.select", opts = { tab = true } },
        ["<C-p>"] = "actions.preview",
        ["q"] = { "actions.close", mode = "n" },
        ["<C-l>"] = false,
        ["-"] = { "actions.parent", mode = "n" },
        ["_"] = { "actions.open_cwd", mode = "n" },
        ["`"] = { "actions.cd", mode = "n" },
        ["g~"] = { "actions.cd", opts = { scope = "tab" }, mode = "n" },
        ["gs"] = { "actions.change_sort", mode = "n" },
        ["gx"] = "actions.open_external",
        ["g."] = { "actions.toggle_hidden", mode = "n" },
        ["g\\"] = { "actions.toggle_trash", mode = "n" },
    },
})

-- compile-mode.nvim
vim.g.compile_mode = {
    default_command = "make -k ",
    baleia_setup = false,
    bang_expansion = false,
    directory_change_matchers = {},
    error_regexp_table = {},
    error_ignore_file_list = {},
    error_threshold = require("compile-mode").level.WARNING,
    auto_jump_to_first_error = false,
    error_locus_highlight = 500,
    use_diagnostics = false,
    recompile_no_fail = false,
    ask_about_save = true,
    ask_to_interrupt = true,
    buffer_name = "*compilation*",
    time_format = "%a %b %e %H:%M:%S",
    hidden_output = {},
    environment = nil,
    clear_environment = false,
    input_word_completion = true,
    hidden_buffer = false,
    focus_compilation_buffer = true,
    auto_scroll = true,
    use_circular_error_navigation = false,
    debug = false,
    use_pseudo_terminal = false,
}

-- telescope.nvim
require("telescope").setup({
    defaults = require("telescope.themes").get_ivy({
        initial_mode = "insert",
        mappings = {
            i = { ["<Esc>"] = require("telescope.actions").close }
        },
        layout_config = {
            height = 0.35,
            preview_cutoff = 999999,
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

-- Other Plugin Setups
require("hop").setup({})
require("multiple-cursors").setup({})
require("neogit").setup({})
require("tmux").setup({})
-- require('java').setup()

-----------------------------------------------------------
-- LSP & Diagnostics
-----------------------------------------------------------
vim.diagnostic.config({
    virtual_text = false,
})

vim.lsp.enable("bashls")
vim.lsp.enable("clangd")
vim.lsp.enable("gopls")
vim.lsp.enable("lua_ls")
vim.lsp.enable("pyright")
-- vim.lsp.enable('jdtls') -- Enable this for Java support

-----------------------------------------------------------
-- Keymaps
-----------------------------------------------------------
local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }
local telescope_builtin = require("telescope.builtin")

-- General Actions & Buffers
keymap("n", "<leader>bd", ":bd<CR>", opts)
keymap("n", "<leader>a", ":NvimTreeToggle<CR>", opts)
keymap("n", "<leader>n", ":bn<CR>", opts)
keymap("n", "<leader>p", ":bp<CR>", opts)
keymap("n", "<leader>e", ":Oil<CR>", opts)
keymap("n", "<leader>g", ":Neogit<CR>", opts)
keymap("n", "<leader>y", '"+y', opts)
keymap("n", "<leader>c", ":Compile<CR>", opts)
keymap("n", "<leader>t", ":TermToggle<CR>", opts)
keymap("n", "<leader>r", ":lua vim.lsp.buf.rename()<CR>", opts)

-- Multiple Cursors & Hop
keymap({ "n", "x" }, "gz", "<Cmd>MultipleCursorsAddMatches<CR>", opts)
keymap({ "n", "x" }, "<C-n>", "<Cmd>MultipleCursorsAddJumpNextMatch<CR>", opts)
keymap("", "f", function() require("hop").hint_char1({ current_line_only = false }) end, opts)

-- Telescope Keymaps
keymap("n", "<leader>ds", telescope_builtin.diagnostics, opts)
keymap("n", "<leader>fb", telescope_builtin.buffers, opts)
keymap("n", "<leader>ff", telescope_builtin.find_files, opts)
keymap("n", "<leader>fg", telescope_builtin.live_grep, opts)
keymap("n", "<leader>fh", telescope_builtin.help_tags, opts)
keymap("n", "<leader>fm", function() telescope_builtin.man_pages({ sections = { "ALL" } }) end, opts)
keymap("n", "<leader>fr", telescope_builtin.oldfiles, opts)

-- Window Management & Terminal Mode Keymaps
keymap("n", "<C-w>o", "<cmd>MaximizerToggle!<CR>", opts)
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

-- Command-line Navigation
keymap("c", "<C-F>", "<Right>", opts)
keymap("c", "<C-B>", "<Left>", opts)

-----------------------------------------------------------
-- Autocommands
-----------------------------------------------------------
-- Auto-format on save
vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*",
    command = [[lua vim.lsp.buf.format()]],
})

-- Removes trailing whitespaces on save
vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*",
    command = [[%s/\s\+$//e]],
})
