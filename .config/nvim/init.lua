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
        "xiyaowong/transparent.nvim",
        config = function()
            vim.cmd("TransparentEnable")
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
    {
        'nvim-telescope/telescope.nvim',
        dependencies = {
            'nvim-lua/plenary.nvim',
            -- optional but recommended
            { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
        },
        config = function()
            require('telescope').setup {
                defaults = require('telescope.themes').get_ivy {
                    initial_mode = "insert",
                    mappings = {
                        i = {
                            -- Press ESC in insert mode to close Telescope
                            ["<Esc>"] = require('telescope.actions').close,
                        }
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
        end
    }
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
keymap('n', '<leader>y', '"+y')
keymap('n', '<leader>c', ':Compile<CR>')
keymap({'n', 'x'}, 'gz', '<Cmd>MultipleCursorsAddMatches<CR>')
keymap({'n', 'x'}, '<C-n>', '<Cmd>MultipleCursorsAddJumpNextMatch<CR>')
keymap('', 'f', function() require('hop').hint_char1({ current_line_only = false}) end, opts)

-- Telescope.
local telescope_builtin = require('telescope.builtin')
keymap('n', '<leader>ds', telescope_builtin.diagnostics)
keymap('n', '<leader>fb', telescope_builtin.buffers)
keymap('n', '<leader>ff', telescope_builtin.find_files)
keymap('n', '<leader>fg', telescope_builtin.live_grep)
keymap('n', '<leader>fh', telescope_builtin.help_tags)
keymap('n', '<leader>fm', function() telescope_builtin.man_pages({ sections = { 'ALL' } }) end)
keymap('n', '<leader>fr', telescope_builtin.oldfiles)

-- Terminal keymaps.
keymap('n', '<leader>t', ':TermToggle<CR>') -- Custom command that spawn the same terminal in split mode.
keymap("t", "<Esc><Esc>", [[<C-\><C-n>]], opts)
keymap('t', '<C-h>', [[<C-\><C-n><C-w>h]], opts)
keymap('t', '<C-j>', [[<C-\><C-n><C-w>j]], opts)
keymap('t', '<C-k>', [[<C-\><C-n><C-w>k]], opts)
keymap('t', '<C-l>', [[<C-\><C-n><C-w>l]], opts)
keymap("t", '<A-h>', [[<C-\><C-n><C-w><]], opts)
keymap("t", '<A-l>', [[<C-\><C-n><C-w>>]], opts)
keymap("t", '<A-j>', [[<C-\><C-n><C-w>-]], opts)
keymap("t", '<A-k>', [[<C-\><C-n><C-w>+]], opts)
keymap('t', '<C-f>', [[<C-\><C-n><C-f>]], opts)
keymap('t', '<C-b>', [[<C-\><C-n><C-b>]], opts)

-- Newer version of Neovim support this cool syntax for LSP autostart.
-- vim.lsp.config['clangd'] = {
--   cmd = { 'clangd' },
--   filetypes = { 'c' },
-- }
--
-- vim.lsp.enable('clangd')

-- Automatically start LSP servers.
-- Compatible with older version of Neovim, like the 0.10.4 distributed in
-- Debian 13 trixie.
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
vim.api.nvim_create_autocmd('FileType', {
    pattern = 'python',
    callback = function(ev)
        vim.lsp.start({
            name = 'python lsp',
            cmd = { 'pylsp' },
            root_dir = vim.fs.root(ev.buf, { '.git/', 'pyproject.toml', 'setup.py' })
        })
    end,
})
keymap('n', '<leader>r', ':lua vim.lsp.buf.rename()<CR>')
cmd [[autocmd BufWritePre * lua vim.lsp.buf.format()]]

-- Disable annoying diagnostics.
vim.diagnostic.config({
  virtual_text = false,
})

-- Activate insert mode when terminal gets focus.
vim.api.nvim_create_autocmd({ "TermOpen", "BufEnter" }, {
    pattern = { "*" },
    callback = function()
        if vim.opt.buftype:get() == "terminal" then
            vim.cmd(":startinsert")
        end
    end
})

-- TermToggle Implementation.
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
