set nocompatible              " be iMproved, required
filetype off                  " required
" ===================================Vundle=================================
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
"Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html
"Plugin 'L9'
" Git plugin not hosted on GitHub
"Plugin 'git://git.wincent.com/command-t.git' " git repos on your local machine (i.e. when working on your own plugin)
"Plugin 'file:///home/gmarik/path/to/plugin'
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
"Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
" Install L9 and avoid a Naming conflict if you've already installed a
" different version somewhere else.
"Plugin 'ascenator/L9', {'name': 'newL9'}

" plugin to put matching brackets/symbols/tags
Plugin 'Raimondi/delimitMate'

" plugin to use git from vim
Plugin 'tpope/vim-fugitive'

" easy commenting
Plugin 'scrooloose/nerdcommenter'

" tab completer
Plugin 'Valloric/YouCompleteMe'
" config generator for YouCompleteMe
Plugin 'rdnetto/YCM-Generator'

" colorscheme
Plugin 'morhetz/gruvbox'

" as stated, indent guides
Plugin 'nathanaelkane/vim-indent-guides'

" syntax checker
Plugin 'scrooloose/syntastic'

" using % for more symbols/tags
Plugin 'matchit.zip'

" pretty statusline
Plugin 'vim-airline/vim-airline'

" virtualenv manipulations
Plugin 'jmcantrell/vim-virtualenv'

" installes windows-qwerty ukrainian keymap
Plugin 'ukrainian-enhanced.vim'
"
" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
"filetype plugin on syntax enable

" ===================================Generic=================================
"indentation details (may be overriden by ftplugin)
set tabstop=2
set shiftwidth=2
set expandtab
set smarttab

" make vim use "" register for system clipboard
"set clipboard^=unnamedplus,unnamed

" show line numbers on left
set ruler
" show line number, symbol number in status line
set number

" end every line with eol; show tab as spaces; show trailing spaces
set list
set lcs=eol:¬,tab:🞂\ ,trail:⎵

" highlight in file what is typed in search
set incsearch

" set leader key to space
let mapleader=" "

" map q: to :q in normal mode, first opens command history, second is usual
" quit
nnoremap q: :q
" ===================================Syntactics=================================
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
" syntax checkers definition
let g:syntastic_python_checkers = ['pylint']
let g:syntastic_cpp_checkers = ['gcc']
let g:syntastic_cpp_compiler_options = ' -std=c++11'
" disable automatic check
let g:syntastic_mode_map = { 'mode': 'passive', 'active_filetypes': [],'passive_filetypes': [] }

" ===================================Files=================================
" swap files (.swp) in a common location
" // means use the file's full path
set dir=~/.vim/_swap//

" backup files (~) in a common location if possible
set backup
set backupdir=~/.vim/_backup/,~/tmp,.

" turn on undo files, put them in a common location
set undofile
set undodir=~/.vim/_undo/

" restore previous cursor position
au BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") && &filetype != "gitcommit" |
        \ execute("normal `\"") |
    \ endif

" keymap for ukrainian support
" setlocal keymap=ukrainian-enhanced

" ===================================Visual=================================
"colorscheme settings
colorscheme gruvbox
set background=dark
let g:gruvbox_contrast_dark="hard"
let g:gruvbox_contrast_light="light"
set t_Co=256

"don't display netrw header
let g:netrw_banner = 0
"display netrw in tree-like style
let g:netrw_liststyle = 3
"open files in previous window
let g:netrw_browse_split = 2

"indentaion guidelines (from vim-indent-guides)
"enable at startup
let g:indent_guides_enable_on_vim_startup = 1
"set indent guides width to 1
let g:indent_guides_guide_size = 1
"disable default mapping to enable/disable
let g:indent_guides_default_mapping = 0
"specify colors

" airline
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_symbols.crypt = '🔒'
"let g:airline_symbols.maxlinenr = '☰'
let g:airline_symbols.maxlinenr = ''
"let g:airline_symbols.paste = 'ρ'
"let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.spell = 'Ꞩ'
let g:airline_symbols.notexists = '∄'
let g:airline_symbols.whitespace = 'Ξ'

" powerline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ''
let g:airline#extensions#tabline#left_alt_sep = ''


set laststatus=2
set noshowmode

"GUI settings to set font
if has("gui_running")
    set guifont=Source\ Code\ Pro\ 12
    set go=aei
    set lines=60 columns=100
endif

" ===================================Language===============================

"let g:XkbSwitchEnabled = 1
"set keymap=ukrainian-modfied
"set iminsert=0
"set imsearch=0
