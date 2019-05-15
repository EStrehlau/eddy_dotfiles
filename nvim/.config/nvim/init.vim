if &compatible
 set nocompatible
endif

set history=1000
set autoread
let g:mapleader = ","
command W w !sudo tee % > /dev/null

let dein_dir = expand('~/.cache/dein')
let dein_plugin_dir = dein_dir . '/repos/github.com/Shougo/dein.vim'

if !isdirectory(dein_dir)
    call mkdir(dein_dir, "p")
    call system('git clone https://github.com/Shougo/dein.vim "' . dein_plugin_dir . '"')
endif

" Add the dein installation directory into runtimepath
let &runtimepath .= ',' . dein_plugin_dir

if dein#load_state('~/.cache/dein')
 call dein#begin('~/.cache/dein')

 call dein#add('~/.cache/dein')
 call dein#add('Shougo/deoplete.nvim')
 if !has('nvim')
   call dein#add('roxma/nvim-yarp')
   call dein#add('roxma/vim-hug-neovim-rpc')
 endif
 call dein#add('rafi/awesome-vim-colorschemes')
 call dein#add('scrooloose/nerdtree')
 call dein#add('ludovicchabant/vim-gutentags')
 call dein#add('vim-airline/vim-airline')
 call dein#add('tpope/vim-surround')

 call dein#end()
 call dein#save_state()
endif

filetype plugin indent on
syntax enable

if dein#check_install()
  let g:dein#install_max_processes = 1
  call dein#install()
  let g:dein#install_max_processes = 8
endif

colorscheme OceanicNext

set rnu
set wildmenu
set ignorecase
set smartcase
set hlsearch
set incsearch
set tabstop=4
set shiftwidth=4
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
nmap <leader>n :NERDTreeToggle<CR>
nnoremap <leader>cd :cd %:p:h<CR>:pwd<CR>

set noerrorbells
set novisualbell
set tm=500
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





