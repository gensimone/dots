local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

local opt= vim.opt
local cmd = vim.cmd
local diagnostic = vim.diagnostic.config
opt.cursorline = false
opt.expandtab = true
opt.hlsearch = false
opt.ignorecase = true
opt.incsearch = true
opt.number = true
opt.relativenumber = true
opt.laststatus = 0
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

vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*",
  command = [[%s/\s\+$//e]],
})

require("lazy").setup({
  performance = { rtp = { reset = false } },
  spec = {
    {
	"ej-shafran/compile-mode.nvim",
        version = "^5.0.0",
        dependencies = { "nvim-lua/plenary.nvim" },
        config = function()
		vim.g.compile_mode = {}
	end
    },
    {
         "aserowy/tmux.nvim",
         config = function()
             require('tmux').setup()
         end
    },
    {
        "smoka7/hop.nvim",
        version = "*",
        config = function()
            require("hop").setup {
                keys = 'etovxqpdygfblzhckisuran'
            }
        end
    },
    {
        'stevearc/oil.nvim',
        opts = {
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
        },
        lazy = false,
    },
    {
        "brenton-leighton/multiple-cursors.nvim",
        version = "*",
        opts = {}
    },
    {
        'NeogitOrg/neogit',
        lazy = true,
        dependencies = {
            'nvim-lua/plenary.nvim',
            'sindrets/diffview.nvim',
            'nvim-telescope/telescope.nvim',
        },
        cmd = 'Neogit'
    },
  }
})

-- Keymaps.
local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }
keymap('n', '<leader>bd', ':bd<CR>')
keymap('n', '<leader>n', ':bn<CR>')
keymap('n', '<leader>p', ':bp<CR>')
keymap('n', '<leader>e', ':Oil<CR>')
keymap('n', '<leader>g', ':Neogit<CR>')
keymap('n', '<leader>y', '"+yy')
keymap('n', '<leader>t', ':term<CR>')
keymap('n', '<leader>c', ':Compile<CR>')
keymap({'n', 'x'}, 'gz', '<Cmd>MultipleCursorsAddMatches<CR>')
keymap({'n', 'x'}, '<C-n>', '<Cmd>MultipleCursorsAddJumpNextMatch<CR>')
keymap('', 'f', function() require('hop').hint_char1({ current_line_only = false}) end, opts)

-- Terminal keymaps.
keymap("t", "<Esc><Esc>", [[<C-\><C-n>]], opts)
keymap('t', '<C-h>', [[<C-\><C-n><C-w>h]], opts)
keymap('t', '<C-j>', [[<C-\><C-n><C-w>j]], opts)
keymap('t', '<C-k>', [[<C-\><C-n><C-w>k]], opts)
keymap('t', '<C-l>', [[<C-\><C-n><C-w>l]], opts)
keymap("t", '<A-h>', [[<C-\><C-n><C-w><]], opts)
keymap("t", '<A-l>', [[<C-\><C-n><C-w>>]], opts)
keymap("t", '<A-j>', [[<C-\><C-n><C-w>-]], opts)
keymap("t", '<A-k>', [[<C-\><C-n><C-w>+]], opts)

-- Nvim >= 0.11
-- vim.lsp.config['clangd'] = {
--   cmd = { 'clangd' },
--   filetypes = { 'c' },
-- }
--
-- vim.lsp.enable('clangd')

-- This works on debian (Nvim 0.10.4)
vim.api.nvim_create_autocmd('FileType', {
    pattern = 'c',
    callback = function(ev)
        vim.lsp.start({
            name = 'clangd',
            cmd = { 'clangd' },
            root_dir = vim.fs.root(ev.buf, { '.git/', '.clang-format', 'Makefile' })
        })
    end,
})
keymap('n', '<leader>r', ':lua vim.lsp.buf.rename()<CR>')
cmd [[autocmd BufWritePre * lua vim.lsp.buf.format()]]

