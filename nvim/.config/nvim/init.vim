if &compatible
 set nocompatible
endif

set history=1000
let g:mapleader = ","
command W w !sudo tee % > /dev/null

let plug_file = stdpath('data') . '/site/autoload/plug.vim'
let plug_file_exists = filereadable(plug_file)

if !plug_file_exists
  call system('curl -fLo ' . plug_file . ' --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim')
endif

call plug#begin(stdpath('data') . '/plugged')
Plug 'rafi/awesome-vim-colorschemes'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'kana/vim-submode'
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'mbbill/undotree'
call plug#end()
if !plug_file_exists
  PlugInstall
endif

filetype plugin indent on
syntax enable

"colorscheme OceanicNext

set rnu
set wildmenu
set ignorecase
set smartcase
set hlsearch
set incsearch
set tabstop=2
set shiftwidth=2
set expandtab
set autoindent
set smartindent
set showmatch
set mat=2
set nobackup
set nowb
set noswapfile
map j gj
map k gk
map <silent> <leader><cr> :noh<cr>
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l
map <leader>bd :Bclose<cr>
nmap <silent> <leader>d :close<CR>
nmap <silent> <leader>v :vs<CR>
nmap <silent> <leader>s :split<CR>
nmap <silent> <leader>b :Buffers<CR>
nmap <silent> <leader>f :Files<CR>
nmap <silent> <leader>c :Rg<CR>
nmap <silent> <leader>/ :Lines<CR>
nmap <silent> <leader>u :UndotreeShow<CR>:UndotreeFocus<CR>
nnoremap <leader>cd :cd %:p:h<CR>:pwd<CR>
tnoremap <Esc> <C-\><C-n>

set noerrorbells
set novisualbell
set tm=500

function! NumToSignedString(num)
  if a:num > 0
    return '+' . a:num
  else
    return '' . a:num
  endif
endfunc

function! ResizeWindow(dir,amount)
  if a:dir ==? 'h'
    let l:resizeCommand = 'vertical resize '
    let l:amount = a:amount
  elseif a:dir ==? 'j'
    let l:resizeCommand = 'resize '
    let l:amount = -a:amount
  elseif a:dir ==? 'k'
    let l:resizeCommand = 'resize '
    let l:amount = a:amount
  elseif a:dir ==? 'l'
    let l:amount = -a:amount
    let l:resizeCommand = 'vertical resize '
  endif
  if l:amount == 0
    return
  endif
  let l:amount = NumToSignedString(l:amount)
  let l:lastWindow = winnr()
  exe 'winc ' . a:dir
  let l:currentWindow = winnr()
  if l:currentWindow != l:lastWindow
    exe l:resizeCommand . l:amount
    winc p
  endif
endfunc

let g:submode_timeout = 0
call submode#enter_with('moveLeft', 'n', '', '<leader>h')
call submode#map('moveLeft', 'n', 's', 'l', ':call ResizeWindow("h", 4)<CR>')
call submode#map('moveLeft', 'n', 's', 'h', ':call ResizeWindow("h", -4)<CR>')
call submode#leave_with('moveLeft', 'n', '', '<ESC>')
call submode#enter_with('moveRight', 'n', '', '<leader>l')
call submode#map('moveRight', 'n', 's', 'l', ':call ResizeWindow("l", 4)<CR>')
call submode#map('moveRight', 'n', 's', 'h', ':call ResizeWindow("l", -4)<CR>')
call submode#leave_with('moveRight', 'n', '', '<ESC>')
call submode#enter_with('moveTop', 'n', '', '<leader>k')
call submode#map('moveTop', 'n', 's', 'j', ':call ResizeWindow("k", 4)<CR>')
call submode#map('moveTop', 'n', 's', 'k', ':call ResizeWindow("k", -4)<CR>')
call submode#leave_with('moveTop', 'n', '', '<ESC>')
call submode#enter_with('moveBottom', 'n', '', '<leader>j')
call submode#map('moveBottom', 'n', 's', 'j', ':call ResizeWindow("j", 4)<CR>')
call submode#map('moveBottom', 'n', 's', 'k', ':call ResizeWindow("j", -4)<CR>')
call submode#leave_with('moveBottom', 'n', '', '<ESC>')


nnoremap <leader>. :call NumberToggle()<cr>
function! NumberToggle()
 if(&relativenumber == 1)
    set number
    set norelativenumber
  else
    set relativenumber
    set nonumber
  endif
endfunc

