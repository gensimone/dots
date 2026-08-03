" ==============================================================================
" BOOTSTRAP VIM-PLUG
" ==============================================================================

" Automatically download vim-plug if it isn't installed
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" ==============================================================================
" CORE OPTIONS & GLOBALS
" ==============================================================================

let mapleader = " "
let maplocalleader = "\\"

set clipboard=unnamedplus
set guicursor=n-v-c-i:block-blinkwait1000-blinkon500-blinkoff500
set nocursorline
set expandtab
set nohlsearch
set ignorecase
set incsearch
set number
set relativenumber
set laststatus=2
set scrolloff=8
set shiftwidth=4
set signcolumn=no
set smartcase
set smartindent
set tabstop=4
set termguicolors
set undofile
set updatetime=300
set nowrap
set fillchars=eob:\

set noshowmode
set noshowcmd
set noruler
set shortmess+=I


" ==============================================================================
" PLUGINS MANAGEMENT (vim-plug)
" ==============================================================================

call plug#begin('~/.vim/plugged')

" File Explorers & Navigation
Plug 'preservim/nerdtree'
Plug 'easymotion/vim-easymotion'

" Fuzzy Finder (Telescope alternative for Vim)
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Git & Terminal Integration
Plug 'tpope/vim-fugitive'
Plug 'voldikss/vim-floaterm'
Plug 'christoomey/vim-tmux-navigator'

" Auto-completion & LSP support (CoC handles LSP, auto-complete, and diagnostics)
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Utility & Themes
Plug 'tribela/vim-transparent'

call plug#end()


" ==============================================================================
" PLUGIN CONFIGURATIONS
" ==============================================================================

" --- NERDTree (nvim-tree equivalent) ---
let g:NERDTreeShowHidden = 1
let g:NERDTreeIgnore = ['^\.git$']

" --- Transparent Vim ---
let g:transparent_enabled = 1

" --- Floating Terminal (toggleterm equivalent) ---
let g:floaterm_keymap_toggle = '<C-t>'
let g:floaterm_width = 0.8
let g:floaterm_height = 0.4
let g:floaterm_position = 'bottom'

" --- CoC / LSP & Auto-completion Setup ---
" Use <CR> to confirm completion
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Auto-install common LSPs dynamically
let g:coc_global_extensions = [
\  'coc-json',
\  'coc-tsserver',
\  'coc-pyright',
\  'coc-clangd',
\  'coc-go',
\  'coc-sh',
\  'coc-lua'
\ ]


" ==============================================================================
" KEYMAPS
" ==============================================================================

" --- General Actions & Buffers ---
nnoremap <silent> <leader>bd :bdelete<CR>
nnoremap <silent> <leader>a  :NERDTreeToggle<CR>
nnoremap <silent> <leader>n  :bnext<CR>
nnoremap <silent> <leader>p  :bprevious<CR>
nnoremap <silent> <leader>e  :Explore<CR>
nnoremap <silent> <leader>g  :Git<CR>
nnoremap <leader>y          "+y
nnoremap <silent> <leader>c  :make<CR>
nnoremap <silent> <leader>t  :FloatermToggle<CR>
nmap <leader>r               <Plug>(coc-rename)

" --- EasyMotion (Hop equivalent) ---
map f <Plug>(easymotion-bd-f)

" --- FZF Keymaps (Telescope equivalent) ---
nnoremap <silent> <leader>ds :CocList diagnostics<CR>
nnoremap <silent> <leader>fb :Buffers<CR>
nnoremap <silent> <leader>ff :Files<CR>
nnoremap <silent> <leader>fg :Rg<CR>
nnoremap <silent> <leader>fh :Helptags<CR>
nnoremap <silent> <leader>fr :History<CR>

" --- Window Management & Terminal Mode ---
tnoremap <Esc><Esc> <C-\><C-n>
tnoremap <C-h>     <C-\><C-n><C-w>h
tnoremap <C-j>     <C-\><C-n><C-w>j
tnoremap <C-k>     <C-\><C-n><C-w>k
tnoremap <C-l>     <C-\><C-n><C-w>l

" --- Command-line Navigation ---
cnoremap <C-F> <Right>
cnoremap <C-B> <Left>


" ==============================================================================
" AUTOCOMMANDS
" ==============================================================================

augroup CustomAutocmds
    autocmd!
    " Auto-format on save via CoC/LSP
    autocmd BufWritePre * silent! call CocAction('format')

    " Remove trailing whitespaces on save
    autocmd BufWritePre * %s/\s\+$//e
augroup END
