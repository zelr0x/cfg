" ## OS-detection
" https://vi.stackexchange.com/a/2577
if !exists('g:os') && !exists('g:is_unix')
    if has('win64') || has('win32') || has('win16')
        let g:os = 'Windows'
        let g:is_unix = 0
    else
        let g:os = substitute(system('uname'), '\n', '', '')
        let g:is_unix = 1
    endif
endif

" ## vim-plug plugin manager

" Start vim-plug
"call plug#begin('~\.vim\plugged')

" List plugins before plug#end()
" Save and type :PlugInstall after adding a plugin in the list below

" Status bar and tab bar
"Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'

" Git wrapper
"Plug 'tpope/vim-fugitive'
" Declare color schemes / make them loadable
"Plug 'hzchirs/vim-material'
"Plug 'morhetz/gruvbox'
"Plug 'altercation/vim-colors-solarized'
"
" Init plugin system; updates $runtimepath and execute { syntax enable, filetype indent on }
"call plug#end()

""" System
set fileformat=unix
set encoding=utf-8
set noundofile

" reduce delay after switching from insert to normal mode
set ttimeoutlen=10

" Protect against modeline pwn
set nomodeline

""" Indentation
" Convert tabs to spaces
:set expandtab
:set tabstop=4
:set shiftwidth=4
""" Disable expandtab for Go files
autocmd FileType go setlocal noexpandtab

""" Line number
set number
set relativenumber
set ruler

""" Fix backspace behaviour
"inoremap <Char-0x07F> <BS> 
"nnoremap <Char-0x07F> <BS>
"set backspace=2
set backspace=indent,eol,start

""" Search
set ignorecase  " make search case-insensitive by default
set smartcase   " switch to case-sensitive upon encountering capital letter

set hlsearch
set incsearch
""" Clear highlighted search results on RET
:nnoremap <CR> :nohlsearch<CR>/<BS>

""" Force write
if g:is_unix
    cmap w!! w !sudo tee > /dev/null %
endif

" ### Font
"if has("gui_running")
"    "set guifont=DejaVu\ Sans\ Mono\ for\ Powerline:h14:cANSI
"    set guifont=Monaco\ for\ Powerline:h12:cANSI
"    set renderoptions=type:directx,renmode:5
"elseif g:os == 'Windows'
"    "set guifont=Monaco\ for\ Powerline:h14:cANSI
"    "set renderoptions=type:directx,renmode:5
"elseif g:os == 'Darwin'
"    set guifont=Monaco\ for\ Powerline:h24
"endif

""" Color
syntax on

if !has("gui_running")
    """ ConEmu (cmder) has 24-bit (true) colors enabled, which interferes with vim.
    set t_Co=256
    if has("win32") && exists("$ConEmuTask")
        "set termguicolors
    endif
endif


""" old stuff: remove or fix
" set default command line to not show current mode (interferes with some plugins)
"set noshowmode

" Colorscheme config and init (GUI)
"if has("gui_running") 
"    :colorscheme solarized
"    let g:airline_theme='solarized' " theme
"    :set guioptions-=T  " remove toolbar
"    :set guioptions-=r  " remove right scroll bar
"    :set lines=50 columns=120 " set initial window size
"endif

"set t_Co=256 
"colorscheme solarized
"set background=dark " for themes like gruvbox
"let g:solarized_termtrans=1 " for solatized in transparent terminal

" ### Misc
" Wrong highlight fix
"let g:solarized_underline=0
"highlight Visual term=reverse cterm=reverse ctermbg=12 ctermfg=0

" ## Plugin configuration

" Airline customization
"let g:airline_theme='solarized' " theme
"let g:airline_solarized_bg='dark'
"let g:airline_powerline_fonts=1 " enable fonts
"let g:Powerline_symbols='unicode' " force unicode for powerline symbols

