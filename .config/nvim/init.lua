vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

local opt= vim.opt
local cmd = vim.cmd
local diagnostic = vim.diagnostic.config

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
cmd("colorscheme lunaperche")

diagnostic({ underline = false })

vim.pack.add({
    "https://github.com/NeogitOrg/neogit",
    "https://github.com/aserowy/tmux.nvim",
    "https://github.com/brenton-leighton/multiple-cursors.nvim",
    "https://github.com/ej-shafran/compile-mode.nvim",
    "https://github.com/neovim/nvim-lspconfig",
    "https://github.com/nvim-lua/plenary.nvim",
    "https://github.com/nvim-telescope/telescope.nvim",
    "https://github.com/smoka7/hop.nvim",
    "https://github.com/stevearc/oil.nvim",
    "https://github.com/xiyaowong/transparent.nvim",
})

require("oil").setup({
    watch_for_changes = true,
    default_file_explorer = true,
    columns = { "permissions", "size", "mtime" },
    view_options = { show_hidden = false },
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

vim.g.compile_mode = {} -- Required by compile-mode.nvim
require("hop").setup({})
require("multiple-cursors").setup({})
require("neogit").setup({})
require("tmux").setup({})
require("telescope").setup {
    defaults = require("telescope.themes").get_ivy {
        initial_mode = "insert",
        mappings = {
            i = { ["<Esc>"] = require("telescope.actions").close }
        },
        layout_config = {
            height = 0.35,
            preview_cutoff = 9999999,
        }
    },
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
}

local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }
local telescope_builtin = require('telescope.builtin')

keymap("n", "<leader>bd", ":bd<CR>")
keymap("n", "<leader>n", ":bn<CR>")
keymap("n", "<leader>p", ":bp<CR>")
keymap("n", "<leader>e", ":Oil<CR>")
keymap("n", "<leader>g", ":Neogit<CR>")
keymap("n", "<leader>y", '"+y')
keymap("n", "<leader>c", ":Compile<CR>")
keymap({"n", "x"}, "gz", "<Cmd>MultipleCursorsAddMatches<CR>")
keymap({"n", "x"}, "<C-n>", "<Cmd>MultipleCursorsAddJumpNextMatch<CR>")
keymap("", "f", function() require("hop").hint_char1({ current_line_only = false}) end, opts)
keymap("n", "<leader>ds", telescope_builtin.diagnostics)
keymap("n", "<leader>fb", telescope_builtin.buffers)
keymap("n", "<leader>ff", telescope_builtin.find_files)
keymap("n", "<leader>fg", telescope_builtin.live_grep)
keymap("n", "<leader>fh", telescope_builtin.help_tags)
keymap("n", "<leader>fm", function() telescope_builtin.man_pages({ sections = { "ALL" } }) end)
keymap("n", "<leader>fr", telescope_builtin.oldfiles)
keymap("n", "<leader>t", ":TermToggle<CR>")
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
keymap("n", "<leader>r", ":lua vim.lsp.buf.rename()<CR>")

vim.lsp.enable("clangd")
vim.lsp.enable("pyright")
vim.lsp.enable("bashls")
vim.lsp.enable("gopls")
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*",
  command = [[lua vim.lsp.buf.format()]],
})

vim.diagnostic.config({
  virtual_text = false,
})

vim.api.nvim_create_autocmd({ "TermOpen", "BufEnter" }, {
    pattern = { "*" },
    callback = function()
        if vim.opt.buftype:get() == "terminal" then
            vim.cmd(":startinsert")
        end
    end
})

local term_buf = nil
local term_win = nil
function OpenOrReuseTerminal()
  if term_buf and vim.api.nvim_buf_is_valid(term_buf) then
    for _, win in ipairs(vim.api.nvim_list_wins()) do
      if vim.api.nvim_win_get_buf(win) == term_buf then
        vim.api.nvim_set_current_win(win)
        return
      end
    end
    vim.cmd("split")
    vim.api.nvim_set_current_buf(term_buf)
  else
    vim.cmd("split | term")
    term_buf = vim.api.nvim_get_current_buf()
  end
end
vim.api.nvim_create_user_command("TermToggle", OpenOrReuseTerminal, {})

vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*",
  command = [[%s/\s\+$//e]],
})
