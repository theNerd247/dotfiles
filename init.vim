" add the directory to 'runtimepath'
set nocompatible

" vim-plug
"-- PLUGINS {{{ ------------------------------------------------------
call plug#begin('~/.vim/bundle')

" file browser
Plug 'scrooloose/nerdtree'
" html tag writer
" Plug 'rstacruz/sparkup'
" snippets
Plug 'vim-scripts/UltiSnips'
" for commenting in files
Plug 'scrooloose/nerdcommenter'
" a fancy status bar
Plug 'bling/vim-airline'
" syntax linting manager
Plug 'neomake/neomake'
" a git interface
Plug 'tpope/vim-fugitive'
" an alignment tool (for right alignment)
Plug 'godlygeek/tabular'
" a parenthesis, brackets, etc. ...thing
Plug 'Shougo/deoplete.nvim'
Plug 'tpope/vim-surround'
" for fancy terminal colors (possibly not needed with better terminals
" available)
Plug 'altercation/vim-colors-solarized'
" for external formatting programs (like astyle)
Plug 'Chiel92/vim-autoformat'
" a code analyzer for vim (requires npm and nodejs)
Plug 'ternjs/tern_for_vim'
" a fuzzy finder for files
Plug 'kien/ctrlp.vim'
" dev tools for haskell
" haskell dev platform
Plug 'parsonsmatt/intero-neovim'
" for haskell formatting
Plug 'neovimhaskell/haskell-vim'
" for haskell autocomplete
"Plug 'eagletmt/neco-ghc'
" auto completion (better than YouCompleteMe)
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' } | Plug 'roxma/nvim-yarp' | Plug 'roxma/vim-hug-neovim-rpc'

call plug#end()

"-- END PLUGINS }}} --------------------------------------------------

" set map leader for custom key-maps
let mapleader = "," 

"-- CUSTOM FUNCTIONS {{{ ---------------------------------------------
function! EscapeString (string)
  let string=a:string
  " Escape regex characters
  let string = escape(string, '^$.*\/~[]')
  " Escape the line endings
  let string = substitute(string, '\n', '\\n', 'g')
  return string
endfunction

" Get the current visual block for search and replaces
" This function passed the visual block through a string escape function
" Based on this - http://stackoverflow.com/questions/676600/vim-replace-selected-text/677918#677918
function! GetVisual() range
  " Save the current register and clipboard
  let reg_save = getreg('"')
  let regtype_save = getregtype('"')
  let cb_save = &clipboard
  set clipboard&

  " Put the current visual selection in the " register
  normal! ""gvy
  let selection = getreg('"')

  " Put the saved registers and clipboards back
  call setreg('"', reg_save, regtype_save)
  let &clipboard = cb_save

  "Escape any special characters in the selection
  let escaped_selection = EscapeString(selection)

  return escaped_selection
endfunction
"-- END CUSTOM FUNCTIONS }}} -----------------------------------------

"-- CUSTOM MAPINGS {{{ -----------------------------------------------
" open the output from the compiler
nmap <leader>co :copen<cr><C-w><S-j>
" close the output from the compiler
nmap <leader>cc :cclose<cr>
" replace selected text
vmap <leader>s <Esc>:%s/<C-r>=GetVisual()<cr>/
" open the compiler output
nmap <leader>sc :copen<cr>
" automagically set the current build path using NERDTree
nmap <leader>nb cd:let $MAKEDIR=getcwd()<cr>Pcd
" manually set the current build path
nmap <leader>sb :let $MAKEDIR=getcwd()<cr>
" shows the buffers 
nmap <leader>bb :buffers<cr>
" center current line on screen
nmap <space> zt
" run make
nmap <leader>m :make<cr>
" fix indenting of a file (see :help =)
nmap <leader>rf :Autoformat <cr>
" vertical split
nmap <leader>vs :vsp<CR>
" horizontal split
nmap <leader>hs :sp<CR>
" write the file 
nmap <leader>w :w<cr>
" edit the vimrc file
nmap <leader>se :tabnew ~/.config/nvim/init.vim<cr>
" shortcut to help screen in new tab
nmap <leader>hh :tab help 
"edit Ultisnips 
nmap <leader>su :tabnew ~/.vim/UltiSnips/ <cr>
nmap <leader>q :q<cr>
nmap <leader>tn :tabnew<cr>
nmap <leader>td :tabclose<cr>
" buffer controls
nmap <leader>bn :bn<cr>
nmap <leader>bp :bp<cr>
nmap <leader>bd :bd<cr>
"-- END CUSTOM MAPINGS }}} -------------------------------------------

"-- CUSTOM MACROS {{{ ------------------------------------------------
"-- END CUSTOM MACROS }}} --------------------------------------------

"-- PLUGIN CONFIG {{{ -------------------------------------------------
" close the screen of nerdtree and tagbar
nmap <leader><SPACE> :NERDTreeClose<cr> :cclose<cr>
" delimitMate mappings
" alias for <S-Tab>
imap <C-j> <S-Tab>
" show buffers and tabs in airline plugin
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#syntastic#enabled = 1

"deoplete
let g:deoplete#enable_at_startup = 1

"nerdtree mappings 
nmap <leader>nn :NERDTreeToggle <cr>
nnoremap <leader>nf :NERDTreeFind<cr>

"-- END PLUGIN CONFIG }}} ---------------------------------------------

"-- PRINTER {{{ ------------------------------------------------------
set printoptions=top:1in,bottom:1in,left:0.5in,right:0.5in
set printheader=" "
"-- END PRINTER }}} --------------------------------------------------

"-- MISC {{{ ---------------------------------------------------------
"omnicomplete options
"set omnifunc=syntaxcomplete#Complete
"set completeopt+=longest

" set the mode of mouse
set mouse=a

" I like syntax highlighting
syntax enable

" set custom colors
"use 16 colors
set t_Co=16 
"set this ONLY if you're using the solarized theme
let g:solarized_termcolors=16
set background=dark
colorscheme solarized

" show the commands as I type them
set showcmd

" don't wrap text by default
set nowrap

" allow for inline searching
set incsearch

" fold code by syntax
set foldmethod=syntax

" default tab size
set tabstop=2
" spaces to use for auto-indent
set shiftwidth=2
" number of spaces for a single tab
set softtabstop=2
set expandtab

" show lines numbers
set number
set relativenumber

" set ctags path
set tags+=/usr/include/tags

" i don't like to scroll horizontally so much
set textwidth=80

" turn off smart indenting
set si

" turn off spell check
set nospell

" set the program to use for the S-K mapping
set keywordprg=

" set the number of lines to buffer the cursor with (above or below) when
" scrolling
set scrolloff=10

"-- END MISC }}} -----------------------------------------------------
