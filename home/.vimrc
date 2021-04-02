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

" Sudo write from the active editor
if g:is_unix
    cmap w!! w !sudo tee > /dev/null %
endif

" ## vim-plug plugin manager

" Start vim-plug
call plug#begin('~\.vim\plugged')

" List plugins before plug#end()
" Save and type :PlugInstall after adding a plugin in the list below

" Status bar and tab bar
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Git wrapper
Plug 'tpope/vim-fugitive'
" Declare color schemes / make them loadable
Plug 'hzchirs/vim-material'
Plug 'morhetz/gruvbox'
Plug 'altercation/vim-colors-solarized'
"
" Init plugin system; updates $runtimepath and execute { syntax enable, filetype indent on }
call plug#end()

" ## Default UI

" Default encoding
set encoding=utf-8

" Show line numbers
:set number relativenumber

" set default command line to not show current mode
set noshowmode

" reduce delay after switching from insert to normal mode
set ttimeoutlen=10

" Commented out since those are issued by plug#end()
" syntax off
" filetype indent off

" ### Indentation
" Convert tabs to spaces
:set expandtab
:set tabstop=4
:set shiftwidth=4

" ### Font
if has("gui_running")
    "set guifont=DejaVu\ Sans\ Mono\ for\ Powerline:h14:cANSI
    set guifont=Monaco\ for\ Powerline:h12:cANSI
    set renderoptions=type:directx,renmode:5
elseif g:os == 'Windows'
    "set guifont=Monaco\ for\ Powerline:h14:cANSI
    "set renderoptions=type:directx,renmode:5
elseif g:os == 'Darwin'
    set guifont=Monaco\ for\ Powerline:h24
endif

" ### Search
" Make case-insensitive by default.
" Use \C anywhere in a query to make the search case-sensitive.
set ignorecase
" Case-sensitive when any capital letters present
set smartcase

" ### Key mapping
" Fix backspace behaviour
inoremap <Char-0x07F> <BS> 
nnoremap <Char-0x07F> <BS>
set backspace=2
set backspace=indent,eol,start

" ### Color
" Colorscheme config and init (GUI)
if has("gui_running") 
    :colorscheme solarized
    let g:airline_theme='solarized' " theme
    :set guioptions-=T  " remove toolbar
    :set guioptions-=r  " remove right scroll bar
    :set lines=50 columns=120 " set initial window size
endif

set t_Co=256 
"colorscheme solarized
set background=dark " for themes like gruvbox
"let g:solarized_termtrans=1 " for solatized in transparent terminal

" ### Misc
" Wrong highlight fix
let g:solarized_underline=0
highlight Visual term=reverse cterm=reverse ctermbg=12 ctermfg=0

" ## Plugin configuration

" Airline customization
"let g:airline_theme='solarized' " theme
"let g:airline_solarized_bg='dark'
let g:airline_powerline_fonts=1 " enable fonts
let g:Powerline_symbols='unicode' " force unicode for powerline symbols

