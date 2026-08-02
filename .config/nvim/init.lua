-- Options.
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

local opt = vim.opt
local cmd = vim.cmd
local diagnostic = vim.diagnostic.config
opt.clipboard = "unnamedplus"
opt.guicursor = "n-v-c-i:block-blinkwait1000-blinkon500-blinkoff500";
opt.cursorline = false opt.expandtab = true
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
opt.fillchars = {eob = " "}
cmd("set noshowmode")
cmd("set noshowcmd")
cmd("set noruler")
cmd("set shortmess+=I")
diagnostic({ underline = false })

-- Plugins.
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

-- Support for Java and Spring Boot.
-- vim.pack.add({
--   {
--     src = 'https://github.com/JavaHello/spring-boot.nvim',
--     version = '218c0c26c14d99feca778e4d13f5ec3e8b1b60f0',
--   },
--   'https://github.com/MunifTanjim/nui.nvim',
--   'https://github.com/mfussenegger/nvim-dap',
--
--   'https://github.com/nvim-java/nvim-java',
-- })
--
-- require('java').setup()
-- vim.lsp.enable('jdtls')

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

require("toggleterm").setup({
  -- size can be a number or function which is passed the current terminal
  size = function(term)
    if term.direction == "horizontal" then
      return 15
    elseif term.direction == "vertical" then
      return vim.o.columns * 0.4
    end
  end,

  open_mapping = [[<c-t>]], -- or { [[<c-\>]], [[<c-¥>]] } if you also use a Japanese keyboard.
  hide_numbers = true, -- hide the number column in toggleterm buffers
  shade_filetypes = {},
  autochdir = true, -- when neovim changes it current directory the terminal will change it's own when next it's opened
  shade_terminals = true, -- NOTE: this option takes priority over highlights specified so if you specify Normal highlights you should set this to false
  start_in_insert = true,
  insert_mappings = true, -- whether or not the open mapping applies in insert mode
  terminal_mappings = true, -- whether or not the open mapping applies in the opened terminals
  persist_size = true,
  persist_mode = true, -- if set to true (default) the previous terminal mode will be remembered
  direction = 'float', -- 'vertical' | 'horizontal' | 'tab' | 'float'
  close_on_exit = true, -- close the terminal window when the process exits
  clear_env = false, -- use only environmental variables from `env`, passed to jobstart()

   -- Change the default shell. Can be a string or a function returning a string
  shell = vim.o.shell,
  auto_scroll = true, -- automatically scroll to the bottom on terminal output
})

local cmp = require("cmp")
require("cmp").setup({
    window = {},
    mapping = cmp.mapping.preset.insert({
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-Space>"] = cmp.mapping.complete(),
        ["<C-e>"] = cmp.mapping.abort(),
        ["<CR>"] = cmp.mapping.confirm({ select = true }),
    }),
    sources = cmp.config.sources({{ name = "nvim_lsp" }, { name = "path" }})
})

vim.g.transparent_enabled = true

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

---@module "compile-mode"
---@type CompileModeOpts
vim.g.compile_mode = {
    -- The string to show in the compile prompt as a default.
    -- For an empty prompt, you can use:
    -- default_command = "",
    -- To use different defaults based on filetype, you can use a table:
    -- default_command = {
    --   python = "python %",
    --   lua = "lua %",
    --   javascript = "bun %",
    --   typescript = "bun %",
    --   c = "cc -o %:r % && ./%:r",
    --   cpp = "cc -std=c++23 -o %:r % && ./%:r",
    --   java = "javac % && java %:r",
    --   go = "go run %",
    -- },
    -- A function which returns the default command string is also supported:
    -- default_command = function()
    --   local filetype = vim.bo.filetype
    --   if filetype == "python" then
    --     return "python %"
    --   else
    --     return "make -k "
    --   end
    -- end,
    -- :h compile_mode.default_command
    default_command = "make -k ",
    -- Use `baleia` for parsing ANSI escape codes in the output.
    -- :h compile_mode.baleia_setup
    baleia_setup = false,
    -- Expand commands, like `:!` (e.g. `:Compile echo %`)
    -- :h compile_mode.bang_expansion
    bang_expansion = false,
    -- Configure additional entering/leaving directory regexes.
    -- :h compile-mode.directory_change_matchers
    directory_change_matchers = {},
    -- Configure additional error regexes.
    -- :h compile-mode-errors
    error_regexp_table = {},
    -- List of filename regexes to ignore errors from.
    -- :h compile-mode.error_ignore_file_list
    error_ignore_file_list = {},
    -- The minimum error level to jump to.
    -- :h compile-mode.error_threshold
    error_threshold = require("compile-mode").level.WARNING,
    -- Automatically jump to the first error.
    -- :h compile-mode.auto_jump_to_first_error
    auto_jump_to_first_error = false,
    -- How long to highlight an error's location when jumping to it.
    -- :h compile-mode.error_locus_highlight
    error_locus_highlight = 500,
    -- Use Neovim diagnostics instead of opening the compilation buffer.
    -- :h compile-mode.use_diagnostics
    use_diagnostics = false,
    -- Default to calling `:Compile` for `:Recompile`
    -- when there's no previous command.
    -- :h compile-mode.recompile_no_fail
    recompile_no_fail = false,
    -- Ask to save unsaved buffers before compiling.
    -- :h compile-mode.ask_about_save
    ask_about_save = true,
    -- Ask to interrupt already running commands.
    -- :h compile-mode.ask_to_interrupt
    ask_to_interrupt = true,
    -- The name for the compilation buffer.
    -- :h compile-mode.buffer_name
    buffer_name = "*compilation*",
    -- The format for the time information
    -- at the top of the compilation buffer
    -- :h compile-mode.time_format
    time_format = "%a %b %e %H:%M:%S",
    -- List of regexes to hide from the output.
    -- :h compile-mode.hidden_output
    hidden_output = {},
    -- A table of environment variables to pass to commands.
    -- :h compile-mode.environment
    environment = nil,
    -- Clear all environment variables for each command.
    -- :h compile-mode.clear_environment
    clear_environment = false,
    -- Fix compilation for plugins like `nvim-cmp`.
    -- :h compile-mode.input_word_completion
    input_word_completion = true,
    -- Hide the compliation buffer.
    -- :h compile-mode.hidden_buffer
    hidden_buffer = false,
    -- Automatically focus the compilation buffer.
    -- :h compile-mode.focus_compilation_buffer
    focus_compilation_buffer = true,
    -- Automatically move the cursor to the end of the compilation buffer.
    -- :h compile-mode.auto_scroll
    auto_scroll = true,
    -- Jump back past the end/beginning of the errors
    -- with `:NextError`/`:PrevError`
    -- :h compile-mode.use_circular_error_navigation
    use_circular_error_navigation = false,
    -- Print debug information.
    -- :h compile-mode.debug
    debug = false,
    -- Use a pseudo terminal for command execution.
    -- :h compile-mode.use_pseudo_terminal
    use_pseudo_terminal = false,
}

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

-- Keymaps
local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }
local telescope_builtin = require("telescope.builtin")
keymap("n", "<leader>bd", ":bd<CR>")
keymap("n", "<leader>a", ":NvimTreeToggle<CR>")
keymap("n", "<leader>n", ":bn<CR>")
keymap("n", "<leader>p", ":bp<CR>")
keymap("n", "<leader>e", ":Oil<CR>")
keymap("n", "<leader>g", ":Neogit<CR>")
keymap("n", "<leader>y", '"+y')
keymap("n", "<leader>c", ":Compile<CR>")
keymap({ "n", "x" }, "gz", "<Cmd>MultipleCursorsAddMatches<CR>")
keymap({ "n", "x" }, "<C-n>", "<Cmd>MultipleCursorsAddJumpNextMatch<CR>")
keymap("", "f", function() require("hop").hint_char1({ current_line_only = false }) end, opts)
keymap("n", "<leader>ds", telescope_builtin.diagnostics)
keymap("n", "<leader>fb", telescope_builtin.buffers)
keymap("n", "<leader>ff", telescope_builtin.find_files)
keymap("n", "<leader>fg", telescope_builtin.live_grep)
keymap("n", "<leader>fh", telescope_builtin.help_tags)
keymap("n", "<leader>fm", function() telescope_builtin.man_pages({ sections = { "ALL" } }) end)
keymap("n", "<leader>fr", telescope_builtin.oldfiles)
keymap("n", "<leader>t", ":TermToggle<CR>")
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
keymap("n", "<leader>r", ":lua vim.lsp.buf.rename()<CR>")

keymap("c", "<C-F>", "<Right>")
keymap("c", "<C-B>", "<Left>")

vim.lsp.enable("bashls")
vim.lsp.enable("clangd")
vim.lsp.enable("gopls")
vim.lsp.enable("lua_ls")
vim.lsp.enable("pyright")

-- vim.api.nvim_create_user_command("Format", [[lua vim.lsp.buf.format()]], {})
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*",
  command = [[lua vim.lsp.buf.format()]],
})

vim.diagnostic.config({
    virtual_text = false,
})

-- Removes trailing whitespaces on save.
vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*",
    command = [[%s/\s\+$//e]],
})
