-- Bootstrap lazy.nvim
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

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

local opt= vim.opt
local cmd = vim.cmd
local diagnostic = vim.diagnostic.config
opt.clipboard = "unnamedplus"
opt.cursorline = false
opt.expandtab = true
opt.hlsearch = false
opt.ignorecase = true
opt.incsearch = true
opt.number = true
opt.relativenumber = false
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
cmd [[autocmd BufWritePre * lua vim.lsp.buf.format()]]
diagnostic({ underline = false })

-- Rounded corners
vim.o.winborder = "bold"

-- Automatically remove trailing whitespace
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*",
  command = [[%s/\s\+$//e]],
})

-- Plugin installation and configuration.
-- Lazy configuration.
require("lazy").setup({
  spec = {
    {
        "ej-shafran/compile-mode.nvim",
        version = "^5.0.0",
        -- you can just use the latest version:
        -- branch = "latest",
        -- or the most up-to-date updates:
        -- branch = "nightly",
        dependencies = {
            "nvim-lua/plenary.nvim",
            -- if you want to enable coloring of ANSI escape codes in
            -- compilation output, add:
            -- { "m00qek/baleia.nvim", tag = "v1.3.0" },
        },
        config = function()
            ---@type CompileModeOpts
            vim.g.compile_mode = {
                -- if you use something like `nvim-cmp` or `blink.cmp` for completion,
                -- set this to fix tab completion in command mode:
                -- input_word_completion = true,

                -- to add ANSI escape code support, add:
                baleia_setup = true,

                -- to make `:Compile` replace special characters (e.g. `%`) in
                -- the command (and behave more like `:!`), add:
                -- bang_expansion = true,
            }
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
            -- Set to true to watch the filesystem for changes and reload oil
            watch_for_changes = true,
            default_file_explorer = true,
            -- Id is automatically added at the beginning, and name at the end
            -- See :help oil-columns
            columns = {
                -- "icon",
                "permissions",
                "size",
                "mtime",
            },
            view_options = {
                show_hidden = true
            },
            -- Send deleted files to the trash instead of permanently deleting them (:help oil-trash)
            delete_to_trash = false,
            -- Skip the confirmation popup for simple operations (:help oil.skip_confirm_for_simple_edits)
            skip_confirm_for_simple_edits = true,
            -- Selecting a new/moved/renamed file or directory will prompt you to save changes first
            -- (:help prompt_save_on_select_new_entry)
            prompt_save_on_select_new_entry = true,
            -- Keymaps
            keymaps = {
                ["g?"] = { "actions.show_help", mode = "n" },
                ["<CR>"] = "actions.select",
                ["<C-s>"] = { "actions.select", opts = { vertical = true } },
                -- ["<C-h>"] = { "actions.select", opts = { horizontal = true } },
                ["<C-h>"] = false,
                ["<C-t>"] = { "actions.select", opts = { tab = true } },
                ["<C-p>"] = "actions.preview",
                ["q"] = { "actions.close", mode = "n" },
                -- ["<C-l>"] = "actions.refresh",
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

        -- Optional dependencies
        -- dependencies = { { "nvim-mini/mini.icons", opts = {} } },
        -- dependencies = { "nvim-tree/nvim-web-devicons" }, -- use if you prefer nvim-web-devicons
        -- Lazy loading is not recommended because it is very tricky to make it work correctly in all situations.
        lazy = false,
    },
    {
        "neovim/nvim-lspconfig",
        config = function()
            vim.lsp.enable('pyright')
            vim.lsp.enable('clangd')
            vim.lsp.enable('bashls')
        end
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
            'nvim-lua/plenary.nvim',         -- required
            'sindrets/diffview.nvim',        -- optional - Diff integration
            'nvim-telescope/telescope.nvim', -- optional
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
    },
    {
        "xiyaowong/transparent.nvim",
        config = function()
            -- Optional, you don't have to run setup.
            require("transparent").setup({
                -- table: default groups
                groups = {
                    'Normal', 'NormalNC', 'Comment', 'Constant', 'Special', 'Identifier',
                    'Statement', 'PreProc', 'Type', 'Underlined', 'Todo', 'String', 'Function',
                    'Conditional', 'Repeat', 'Operator', 'Structure', 'LineNr', 'NonText',
                    'SignColumn', 'CursorLine', 'CursorLineNr', 'StatusLine', 'StatusLineNC',
                    'EndOfBuffer',
                },
                -- table: additional groups that should be cleared
                extra_groups = {},
                -- table: groups you don't want to clear
                exclude_groups = {},
                -- function: code to be executed after highlight groups are cleared
                -- Also the user event "TransparentClear" will be triggered
                on_clear = function() end,
            })
            vim.cmd('TransparentEnable') -- Enable transparency at boot.
        end
    }
  }, -- spec: close

  -- automatically check for plugin updates
  checker = { enabled = false },
  change_detection = { enabled = false }
})

-- Keymaps
local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }

keymap('n', '<C-h>', '<C-w>h', { desc = 'Move to left window' })
keymap('n', '<C-j>', '<C-w>j', { desc = 'Move to lower window' })
keymap('n', '<C-k>', '<C-w>k', { desc = 'Move to upper window' })
keymap('n', '<C-l>', '<C-w>l', { desc = 'Move to right window' })

-- Telescope
local telescope_builtin = require('telescope.builtin')
keymap('n', '<leader>ff', telescope_builtin.find_files)
keymap('n', '<leader>fr', telescope_builtin.oldfiles)
keymap('n', '<leader>fg', telescope_builtin.live_grep)
keymap('n', '<leader>fb', telescope_builtin.buffers)
keymap('n', '<leader>fh', telescope_builtin.help_tags)
keymap('n', '<leader>fm', function() telescope_builtin.man_pages({ sections = { 'ALL' } }) end)
keymap('n', '<leader>fc', function() telescope_builtin.find_files({ cwd = vim.fn.stdpath('config') }) end)
keymap('n', '<leader>ds', telescope_builtin.diagnostics)

-- Various stuff
keymap('n', '<leader>bd', ':bd<CR>')
keymap('n', '<leader>n', ':bn<CR>')
keymap('n', '<leader>p', ':bp<CR>')
keymap('n', '<leader>e', ':Oil<CR>')
keymap('n', '<leader>g', ':Neogit<CR>')
keymap('n', '<leader>r', ':lua vim.lsp.buf.rename()<CR>')
keymap('n', '<leader>y', '"+y')
keymap('n', '<leader>t', ':term<CR>')
keymap('n', '<leader>c', ':Compile<CR>')
keymap({'n', 'x'}, "gz", "<Cmd>MultipleCursorsAddMatches<CR>")
keymap({'n', 'x'}, "<C-n>", "<Cmd>MultipleCursorsAddJumpNextMatch<CR>")
keymap('', 'f', function() require('hop').hint_char1({ current_line_only = false}) end, opts)

-- Terminal
keymap("t", "<Esc><Esc>", [[<C-\><C-n>]], opts)
keymap('t', '<C-h>', [[<C-\><C-n><C-w>h]], opts)
keymap('t', '<C-j>', [[<C-\><C-n><C-w>j]], opts)
keymap('t', '<C-k>', [[<C-\><C-n><C-w>k]], opts)
keymap('t', '<C-l>', [[<C-\><C-n><C-w>l]], opts)
keymap("t", "<A-h>", [[<C-\><C-n><C-w><]], opts)
keymap("t", "<A-l>", [[<C-\><C-n><C-w>>]], opts)
keymap("t", "<A-j>", [[<C-\><C-n><C-w>-]], opts)
keymap("t", "<A-k>", [[<C-\><C-n><C-w>+]], opts)
