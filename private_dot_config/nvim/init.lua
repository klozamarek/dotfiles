-- Init.lua equivalent of the provided init.vim
-- Utilizing lazy.nvim for plugin management

-- Lazy.nvim bootstrap
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({"git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", lazypath})
end
vim.opt.rtp:prepend(lazypath)

-- Plugins
require("lazy").setup({
  {"morhetz/gruvbox"},
  {"chrisbra/csv.vim"},
  {"moll/vim-bbye"},
  {"simeji/winresizer", init = function() vim.g.winresizer_start_key = "<F2>" end },
  {'junegunn/fzf', build = './install --bin',},
  {'junegunn/fzf.vim'},
  { "simnalamburt/vim-mundo", config = function() vim.keymap.set("n", "<F5>", ":MundoToggle<CR>") end },
  {"jez/vim-superman"},
  {"mcchrish/nnn.vim"},
  { "vim-airline/vim-airline", init = function() vim.g.airline_powerline_fonts = 1 end },
  {"tpope/vim-fugitive"},
  {"nvim-lua/plenary.nvim"},
  {"nvim-telescope/telescope.nvim", tag = "0.1.8", config = function()
        require('telescope').setup {
          extensions = {
            fzf = {
              fuzzy = true,                    -- false will only do exact matching
              override_generic_sorter = true,  -- override the generic sorter
              override_file_sorter = true,     -- override the file sorter
              case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
            },
            chezmoi = {} 
          }
        }
      require("telescope").load_extension("fzf")
      require("telescope").load_extension("chezmoi")
    end },
  {"nvim-treesitter/nvim-treesitter", build = ":TSUpdate"},
  {'akinsho/bufferline.nvim', version = "*", dependencies = 'nvim-tree/nvim-web-devicons',
    config = function()
      require("bufferline").setup{}
    end },
  {"nvim-telescope/telescope-fzf-native.nvim", build = "make"},
  {"ryanoasis/vim-devicons"},
  {"tpope/vim-surround"},
  {"nathanaelkane/vim-indent-guides"},
  {"tpope/vim-eunuch"},
  {"tpope/vim-obsession"},
  {"tpope/vim-commentary"},
  --{"alker0/chezmoi.vim"}
  {"rcarriga/nvim-notify"},
  {'xvzc/chezmoi.nvim', dependencies = { 'nvim-lua/plenary.nvim' }, config = function()
      require("chezmoi").setup {
        -- your configurations
        edit = {
          watch = true, -- Set true to automatically apply on save.
          force = true, -- Set true to force apply. Works only when watch = true.
        },
        notification = {
          on_open = true, -- vim.notify when start editing chezmoi-managed file.
          on_apply = true, -- vim.notify on apply.
          on_watch = false,
        },
        telescope = {
          select = { "<CR>" },
        },
      }
      end},
  {"christoomey/vim-tmux-navigator",
    cmd = {
      "TmuxNavigateLeft",
      "TmuxNavigateDown",
      "TmuxNavigateUp",
      "TmuxNavigateRight",
      "TmuxNavigatePrevious",
      "TmuxNavigatorProcessList",
    },
    keys = {
      { "<c-h>", "<cmd><C-U>TmuxNavigateLeft<cr>" },
      { "<c-j>", "<cmd><C-U>TmuxNavigateDown<cr>" },
      { "<c-k>", "<cmd><C-U>TmuxNavigateUp<cr>" },
      { "<c-l>", "<cmd><C-U>TmuxNavigateRight<cr>" },
      { "<c-\\>", "<cmd><C-U>TmuxNavigatePrevious<cr>" },
    },
  }    
})

-- Configure nvim-notify
local notify = require("notify")
vim.notify = notify
notify.setup({
  stages = "fade_in_slide_out",  -- Animation style
  timeout = 3000,                -- Notification duration in ms
  render = "default",            -- Default notification UI
  background_colour = "#000000",  -- Background color
  max_width = 50,
  max_height = 5,
})

-- General Settings
vim.opt.compatible = false
vim.opt.nrformats = ""
vim.opt.wildmenu = true
vim.opt.wildmode = {"full"}
vim.opt.history = 200
vim.opt.termguicolors = true
vim.opt.foldmethod = "marker"
vim.g.mapleader = " "
vim.g['airline#extensions#tabline#enabled'] = 0

-- Keybindings
vim.api.nvim_set_keymap("n", "<space>", "<nop>", { noremap = true })
vim.api.nvim_set_keymap("n", "<leader><F5>", ":tabedit $MYVIMRC<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader><F6>", ":source $MYVIMRC<CR>", { noremap = true, silent = true })

-- Keybinding for chezmoi.nvim using telescope
vim.keymap.set('n', '<leader>cme', require("telescope").extensions.chezmoi.find_files, {})

-- Clipboard
vim.opt.clipboard:append("unnamedplus")

-- Line numbers
vim.opt.number = true
vim.opt.relativenumber = true

-- File handling
vim.opt.swapfile = false
vim.opt.hidden = true
vim.opt.undofile = true
vim.opt.undodir = vim.fn.expand("~/.config/nvim/undo")
vim.opt.undolevels = 10000
vim.opt.undoreload = 10000

-- trailing special characters
vim.opt.list = true
vim.opt.listchars = "tab:→ ,trail:·,nbsp:␣,eol:↴,space:·,extends:»,precedes:«"

-- Search
vim.api.nvim_set_keymap("n", "<Esc><Esc>", ":nohlsearch<CR><Esc>", { noremap = true, silent = true })
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Keep search results at the center of screen
vim.api.nvim_set_keymap('n', 'n', 'nzz', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'N', 'Nzz', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '*', '*zz', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '#', '#zz', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'g*', 'g*zz', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'g#', 'g#zz', { noremap = true, silent = true })

-- Cursor
vim.opt.cursorline = true
vim.opt.cursorcolumn = true

-- Window navigation mappings
-- vim.api.nvim_set_keymap('n', '<M-h>', '<C-w>h', { noremap = true, silent = true })
-- vim.api.nvim_set_keymap('n', '<M-j>', '<C-w>j', { noremap = true, silent = true })
-- vim.api.nvim_set_keymap('n', '<M-k>', '<C-w>k', { noremap = true, silent = true })
-- vim.api.nvim_set_keymap('n', '<M-l>', '<C-w>l', { noremap = true, silent = true })

-- Terminal mappings specific to Neovim
if vim.fn.has('nvim') == 1 then
  vim.api.nvim_create_autocmd('VimEnter', {
    callback = function()
      vim.api.nvim_set_keymap('t', '<M-h>', [[<C-\><C-n><C-w>h]], { noremap = true, silent = true })
      vim.api.nvim_set_keymap('t', '<M-j>', [[<C-\><C-n><C-w>j]], { noremap = true, silent = true })
      vim.api.nvim_set_keymap('t', '<M-k>', [[<C-\><C-n><C-w>k]], { noremap = true, silent = true })
      vim.cmd([[highlight! link TermCursor Cursor]])
      vim.cmd([[highlight! TermCursorNC guibg=red guifg=white ctermbg=1 ctermfg=15]])
      vim.api.nvim_set_keymap('t', '<Esc>', [[<C-\><C-n>]], { noremap = true, silent = true })
      vim.api.nvim_set_keymap('t', '<C-v><Esc>', '<Esc>', { noremap = true, silent = true })
    end,
  })
end

-- Terminal
vim.api.nvim_set_keymap('n', '<leader><Enter>', ':tabnew<CR>:terminal<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>h<Enter>', ':leftabove vnew<CR>:terminal<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>l<Enter>', ':rightbelow vnew<CR>:terminal<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>k<Enter>', ':leftabove new<CR>:terminal<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>j<Enter>', ':rightbelow new<CR>:terminal<CR>', { noremap = true, silent = true })

-- Copy current file path to clipboard
vim.api.nvim_set_keymap('n', '<leader>%', ":lua vim.fn.setreg('+', vim.fn.expand('%'))<CR>", { noremap = true, silent = true })

-- Select all text
vim.api.nvim_set_keymap('n', 'vA', 'ggVG', { noremap = true, silent = true })

-- Trailing whitespaces
vim.opt.list = true
vim.opt.listchars = { tab = '┆ ', trail = '·', nbsp = '±' }

-- Remove trailing whitespaces in current buffer
vim.api.nvim_set_keymap('n', '<Leader><BS>s', ':%s/[ ]*$//<CR>:nohlsearch<CR>1G', { noremap = true, silent = true })

-- Switch between tabs
for i = 1, 9 do
  vim.api.nvim_set_keymap('n', '<leader>' .. i, i .. 'gt', { noremap = true, silent = true })
end

-- Creating splits with empty buffers in all directions
vim.api.nvim_set_keymap('n', '<Leader>nh', ':leftabove vnew<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>nl', ':rightbelow vnew<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>nk', ':leftabove new<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>nj', ':rightbelow new<CR>', { noremap = true, silent = true })

-- Jump to split in the given direction, or create a new split if it doesn't exist
-- If split in given direction exists - jump, else create new split
function JumpOrOpenNewSplit(direction, split_cmd, open_fzf)
  local current_window = vim.api.nvim_get_current_win() -- Get current window ID
  vim.cmd('wincmd ' .. direction) -- Attempt to move to the split in the given direction
  local new_window = vim.api.nvim_get_current_win() -- Check the new current window ID

  if current_window == new_window then
    -- If still in the same window, create a new split
    vim.cmd(split_cmd)
  end

  -- If `open_fzf` is true, open the FZF Files command
  if open_fzf then
    vim.cmd('Files')
  end
end

-- Key mappings for jumping or creating new splits
vim.api.nvim_set_keymap('n', '<Leader>hh', ":lua JumpOrOpenNewSplit('h', 'leftabove vsplit', false)<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>ll', ":lua JumpOrOpenNewSplit('l', 'rightbelow vsplit', false)<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>kk', ":lua JumpOrOpenNewSplit('k', 'leftabove split', false)<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>jj', ":lua JumpOrOpenNewSplit('j', 'rightbelow split', false)<CR>", { noremap = true, silent = true })

-- Universal closing behavior
function CloseSplitOrDeleteBuffer()
  if vim.fn.winnr('$') > 1 then
    vim.cmd('wincmd c')
  else
    vim.cmd('bdelete!')
  end
end
vim.api.nvim_set_keymap('n', '<Leader>q', ':lua CloseSplitOrDeleteBuffer()<CR>', { noremap = true, silent = true })

-- Delete all hidden buffers
function DeleteHiddenBuffers()
  local tpbl = {}
  local deleted_count = 0
  -- Collect all visible buffers
  for i = 1, vim.fn.tabpagenr('$') do
    vim.list_extend(tpbl, vim.fn.tabpagebuflist(i))
  end
  -- Delete all hidden buffers
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.fn.bufexists(buf) == 1 and not vim.tbl_contains(tpbl, buf) then
      pcall(function()
        vim.cmd('bdelete! ' .. buf)
        deleted_count = deleted_count + 1
      end)
    end
  end
  -- Feedback to the user
  print(deleted_count .. " hidden buffer(s) deleted.")
end

-- Key mapping
vim.api.nvim_set_keymap('n', '<Leader><BS>b', ':lua DeleteHiddenBuffers()<CR>', { noremap = true, silent = true })

-- Copy current file path to clipboard
vim.api.nvim_set_keymap('n', '<leader>%', ":lua vim.fn.setreg('+', vim.fn.expand('%'))<CR>", { noremap = true, silent = true })

-- Indentation
vim.opt.expandtab = true  -- Replace <Tab> with spaces
vim.opt.tabstop = 2       -- Number of spaces that a <Tab> in the file counts for
vim.opt.softtabstop = 2   -- Remove <Tab> symbols as if they were spaces
vim.opt.shiftwidth = 2    -- Indent size for << and >>
vim.opt.shiftround = true -- Round indent to multiple of 'shiftwidth' (for << and >>)

-- Plugin Settings (examples)
vim.cmd([[colorscheme gruvbox]])
vim.api.nvim_set_keymap("n", "<leader>tf", "<cmd>Telescope find_files<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>tg", "<cmd>Telescope live_grep<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>tb", "<cmd>Telescope buffers<CR>", { noremap = true, silent = true })

-- Keybindins for FZF
vim.api.nvim_set_keymap('n', '<Leader>ff', ':Files<CR>', { noremap = true, silent = true }) -- Keybinding for FZF Files
vim.api.nvim_set_keymap('n', '<Leader>fb', ':Buffers<CR>', { noremap = true, silent = true }) -- Keybinding for FZF Buffers
