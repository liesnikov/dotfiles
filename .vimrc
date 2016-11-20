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
Plugin 'Raimondi/delimitMate'

Plugin 'tpope/vim-fugitive'

Plugin 'scrooloose/nerdcommenter'

Plugin 'SuperTab'

Plugin 'morhetz/gruvbox'

Plugin 'python.vim'

Plugin 'scrooloose/syntastic'
"Plugin 'vim-xkbswitch'
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
set tabstop=8
set shiftwidth=8
set noexpandtab

" make vim use "" register for system clipboard
set clipboard^=unnamedplus,unnamed

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
let g:netrw_browse_split = 4
"let g:netrw_altv = 1
"set widnow size to __
let g:netrw_winsize = 20

"highlight every 81-st symbol in line
"highlight ColorColumn ctermbg=fg ctermfg=bg
"call matchadd('ColorColumn', '\%81v', 100)

""add support for changing cursor in different modes in xfce-terminal
"if has("autocmd")
  "au InsertEnter * silent execute "!sed -i.bak -e 's/TERMINAL_CURSOR_SHAPE_BLOCK/TERMINAL_CURSOR_SHAPE_UNDERLINE/' ~/.config/xfce4/terminal/terminalrc"
  "au InsertLeave * silent execute "!sed -i.bak -e 's/TERMINAL_CURSOR_SHAPE_UNDERLINE/TERMINAL_CURSOR_SHAPE_BLOCK/' ~/.config/xfce4/terminal/terminalrc"
  "au VimLeave * silent execute "!sed -i.bak -e 's/TERMINAL_CURSOR_SHAPE_UNDERLINE/TERMINAL_CURSOR_SHAPE_BLOCK/' ~/.config/xfce4/terminal/terminalrc"
"endif

"" change cursor shape from
"if &term =~ '^xterm\\|rxvt'
  "" solid underscore
  "let &t_SI .= "\<Esc>[1 q"
  "" solid block
  "let &t_EI .= "\<Esc>[2 q"
  "" 1 or 0 -> blinking block
  "" 3 -> blinking underscore
  "" Recent versions of xterm (282 or above) also support
  "" 5 -> blinking vertical bar
  "" 6 -> solid vertical bar
"endif

"GUI settings to set font
if has("gui_running")
    set guifont=Source\ Code\ Pro\ 12
endif

" ===================================Language===============================

"let g:XkbSwitchEnabled = 1
"set keymap=ukrainian-modfied
"set iminsert=0
"set imsearch=0
