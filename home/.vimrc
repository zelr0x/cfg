""" System
set encoding=utf-8

" Reduce delay after switching from insert to normal mode
set ttimeoutlen=10

" Disable undo file
set noundofile

" Protect against modeline pwn
set nomodeline


""" Line number
set number          " Show absolute current line number to the left
set relativenumber  " Show relative line numbers to the left
set ruler           " Show current line and column at the status line

set fileformats=unix,dos
set fileformat=unix
augroup new_file_le_pick
  autocmd!
  autocmd BufNewFile *.bat,*.cmd setlocal fileformat=dos
augroup END


""" Status line

" Mixed line endings detection
let b:mixed_endings = 0
function! CheckMixedEndings()
  let l:has_crlf = search('\r', 'nw') > 0
  let l:has_lf   = search('\n', 'nw') > 0

  if l:has_crlf && l:has_lf
    let b:mixed_endings = 1
  else
    let b:mixed_endings = 0
  endif
endfunction

augroup mixed_le_check
  autocmd!
  autocmd BufReadPost,BufEnter * call CheckMixedEndings()
  autocmd TextChanged,TextChangedI * call CheckMixedEndings()
augroup END

" Git branch
let b:git_branch = ''

function! UpdateGitBranch()
  let l:dir = expand('%:p:h')
  let l:branch = system('git -C ' . shellescape(l:dir) . ' rev-parse --abbrev-ref HEAD')
  if v:shell_error
    let b:git_branch = ''
  else
    let b:git_branch = substitute(l:branch, '\n', '', 'g')
  endif
endfunction

augroup git_branch
  autocmd!
  autocmd BufEnter,BufWritePost * call UpdateGitBranch()
augroup END


" Always show status line.
set laststatus=2
" Status line format:
" <START>filename filetype b:git_branch readonly-flag modified-flag
" <RIGHT ALIGN>
" line column virtual-column (percentage%-top-all-bottom) | file encoding< (BOM)> | line endings< (mixed)> <END>
set statusline=%f\ %y\ %{b:git_branch}\ %r\ %m%=%l,%c\ %V\ (%P)\ \|\ %{(&fileencoding!=''?&fileencoding:&encoding)}\ %{&bomb?'(BOM)':''}\|\ %{&fileformat}\ %{b:mixed_endings?'(mixed)':''}\ %{''}


""" Indentation
set expandtab
set tabstop=4
set shiftwidth=4
""" Disable expandtab for Go files
autocmd FileType go setlocal noexpandtab

""" Fix backspace
set backspace=indent,eol,start

""" Search
set ignorecase  " make search case-insensitive by default
set smartcase   " switch to case-sensitive upon encountering capital letter

set hlsearch
set incsearch
""" Clear highlighted search results on RET
:nnoremap <CR> :nohlsearch<CR>/<BS>

""" Force write
cmap w!! w !sudo tee > /dev/null %

""" Colors
syntax on

if !has("gui_running")
    """ ConEmu (cmder) has 24-bit (true) colors enabled, which interferes with vim.
    set t_Co=256
    if has("win32") && exists("$ConEmuTask")
        "set termguicolors
    endif
endif

