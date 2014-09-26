set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'vim-ruby/vim-ruby'
Plugin 'tpope/vim-cucumber'
Plugin 'tpope/vim-fugitive'
Plugin 'bling/vim-airline'
Bundle 'edkolev/tmuxline.vim'
Plugin 'KevinGoodsell/vim-csexact'
Plugin 'scrooloose/nerdtree'
Plugin 'tomasr/molokai'
Plugin 'jlanzarotta/bufexplorer'
Plugin 'dbext.vim'
call vundle#end()            " required

"personal settings
let mapleader=","
set encoding=utf-8
set t_Co=256
set nocompatible
syntax on
filetype on
filetype indent plugin on
set background=dark
colorscheme molokai

"ruby options
autocmd FileType ruby,eruby call s:ruby_settings()
function! s:ruby_settings()
  setlocal tabstop=2 shiftwidth=2 expandtab
  let g:rubycomplete_buffer_loading     = 1
  let g:rubycomplete_classes_in_global  = 1
  let g:rubycomplete_rails              = 1
endfunction



"airline options
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
function! AirlineInit()
  let g:airline_section_a = airline#section#create(['mode',' ','branch'])
  let g:airline_section_b = airline#section#create_left(['ffenc','hunks','%f'])
  let g:airline_section_c = airline#section#create(['filetype'])
  let g:airline_section_x = airline#section#create(['%P'])
  let g:airline_section_y = airline#section#create(['%B'])
  let g:airline_section_z = airline#section#create_right(['%l','%c'])
endfunction
autocmd VimEnter * call AirlineInit() 
set laststatus=2
let g:airline#extensions#tmuxline#enabled = 1
let g:airline_theme = 'molokai'

"autocompletion
set wildmode=list:longest
set wildmenu                "enable ctrl-n and ctrl-p to scroll thru matches
set wildignore=*.o,*.obj,*~ "stuff to ignore when tab completing
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=vendor/rails/**
set wildignore+=vendor/cache/**
set wildignore+=*.gem
set wildignore+=log/**
set wildignore+=tmp/**
set wildignore+=*.png,*.jpg,*.gif

"open files in new tabs
autocmd VimEnter * tab all
autocmd BufAdd * exe 'tablast | tabe "' . expand( "<afile") .'"'

"nerdtree stuff here
autocmd vimenter * if !argc() | NERDTree | endif
map <C-n> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

"tabs
nnoremap th  :tabfirst<CR>
nnoremap tj  :tabnext<CR>
nnoremap tk  :tabprev<CR>
nnoremap tl  :tablast<CR>
nnoremap tt  :tabedit<Space>
nnoremap tn  :tabnext<Space>
nnoremap tm  :tabm<Space>
nnoremap td  :tabclose<CR>
nnoremap tc  :tabnew<CR>
