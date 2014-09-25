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
filetype indent on
filetype plugin on
set background=dark
let g:solarized_termcolors=256
colorscheme grb256

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
let g:airline_theme = 'simple'

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
