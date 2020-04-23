let g:netrw_banner=0        " disable annoying banner
let g:netrw_browse_split=0  " horizontally split window
" let g:netrw_altv=1          " open splits to the right
let g:netrw_winsize=90
let g:netrw_liststyle=3     " tree view
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'
" Per default, netrw leaves unmodified buffers open. This autocommand
" deletes netrw's buffer once it's hidden (using ':q', for example)
autocmd FileType netrw setl bufhidden=delete

"EasyAlign
let g:easy_align_delimiters = { '?': { 'pattern': '?' }, '(': { 'pattern': '(' } }
" start EasyAlign (visual)
xmap ga <Plug>(EasyAlign)
" " start EasyAlign (normal)
nmap ga <Plug>(EasyAlign)

" I like syntax highlighting
syntax enable
" set custom colors
"use 16 colors
set t_Co=16 
colorscheme elflord
set background=light
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
" turn off smart indenting
set nosmartindent
set nocindent
set noautoindent
set indentexpr=
" turn off spell check
set nospell
" set the program to use for the S-K mapping
set keywordprg=
" set the number of lines to buffer the cursor with (above or below) when
" scrolling
set scrolloff=10
" highlight when I've gone past the 80 character width
" ctermbg=lightgrey
highlight ColorColumn cterm=reverse ctermbg=none
call matchadd('ColorColumn', '\%81v', 30)
" set map leader for custom key-maps
let mapleader = "," 
" printer options
set printoptions=top:1in,bottom:1in,left:0.5in,right:0.5in
set printheader=" "
set wildignore+=*.o
set wildignore+=*.lib
set wildignore+=*/node_modules/*
set wildignore+=tags
set wildignore+=result
set wildignore+=*.dyn_*
set wildignore+=*.hi
set wildignore+=*.so
set wildignore+=*.a
set wildignore+=*/dist-newtyle/*
set wildignore+=*/dist/*
" custom mappings
" open quickfix window
nmap <leader>co :copen<cr><C-w><S-j>
" center line on screen
nmap <space> zz
" make
nmap <leader>m :make<cr>
" vertical split
nmap <leader>vs :vsp<CR>
" horizontal split
nmap <leader>hs :sp<CR>
" write
nmap <leader>w :w<cr>
" open this file for editing
nmap <leader>se :tabnew ~/.vimrc<cr>
" open help in new tab
nmap <leader>hh :tab help 
" open new tab
nmap <leader>tn :tabnew<cr>
" close tab
nmap <leader>td :tabclose<cr>
" quit
nmap <leader>q :q<cr>
" next buffer
nmap <leader>bn :bn<cr>
" previous buffer
nmap <leader>bp :bp<cr>
" close buffer
nmap <leader>bd :bd<cr>
" switch to a presentation friend mode
nmap <leader>prez :so ~/.config/nvim/prez.vim<cr>
" clear search highlight
nmap <leader>no :nohlsearch<cr>
" git add
nmap <leader>ga :!git add %<cr><cr>
nmap <leader>gc :!git commit -m "
" git commit
" nmap <leader>gc :!git commit -m
" git status
nmap <leader>gs :!git status<cr>
" git vetical diff
nmap <leader>gd :!git diff <cr>
" git reset to HEAD file
nmap <leader>gr :!git checkout HEAD -- %<cr>L
" open netrw
nmap <leader>nw :Netrw<cr>
" javascript class snippet
nnoremap <leader>class :-1read $HOME/.config/nvim/snippets/js/newJsClass.js<cr>
" javascript model
nnoremap <leader>jsmodel :-1read $HOME/.config/nvim/snippets/js/expressModel.js<cr>:%s/X/
" haskell LANGUAGE pragma
nnoremap <leader>lang :-1read $HOME/.config/nvim/snippets/haskell/language.hs<cr>2f<space>
nnoremap <leader>module :-1read $HOME/.config/nvim/snippets/haskell/module.hs<cr>2f<space>
" merge multiple commit messages in a squash to a bulletted list
nnoremap <leader>gm :%s/^\s\{-}\n\{-}#.*\n\{-}\s\{-}\n//g<cr>:%s/\n\n/\r/g<cr>{j<C-v>]]I* <Esc>
" hoogle the current word
noremap <leader>hg :!hoogle <C-R><C-W><cr>
" vim grep current word
noremap <leader>vg :vimgrep /\<<C-R><C-W>\>/ ./**<cr>
" set keymap to haskell
noremap <leader>kh :set keymap=haskell
noremap <leader>kp :set keymap=pollen
" replace currently highlighted text block with regex
vmap <leader>s y:%s/<C-r>"/
nmap <leader>s, :s/, \{-}/\r,/g
nmap <leader>me :read !grep "const .*=" %<cr>vap :s/^const \(.\{-}\) =.*$/, \1/g<cr>o}<ESC>{jr{
