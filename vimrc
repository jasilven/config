"" plugins
call plug#begin('~/.config/nvim/plugged')
Plug 'cormacrelf/vim-colors-github'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'liuchengxu/vista.vim'
Plug 'kovisoft/paredit'
" Plug 'benmills/vimux'
" Plug 'ludovicchabant/vim-gutentags'
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'cespare/vim-toml'
Plug 'easymotion/vim-easymotion'
" Plug 'itchyny/lightline.vim'
Plug 'jiangmiao/auto-pairs', { 'tag': 'v2.0.0' }
Plug 'joshdick/onedark.vim'
Plug 'lifepillar/vim-solarized8'
Plug 'rust-lang/rust.vim', {'for': 'rust'}
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-fireplace'
Plug 'venantius/vim-cljfmt'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf'
call plug#end()

"" settings
set encoding=utf-8 fileencoding=utf-8 fileencodings=utf-8 spelllang=en_us
set scrolloff=2 tabstop=2 shiftwidth=2 " set t_Co=256 completeopt-=preview 
set nolist norelativenumber noswapfile nobackup noshowmode nowrap noshowcmd nospell 
set termguicolors number cursorline hidden ttyfast ruler ignorecase hlsearch autoread 
set wildmode=list:longest,full 
set mouse=a clipboard=unnamed,unnamedplus guioptions=egmrti
set updatetime=500 undofile
set sessionoptions=blank,curdir,help,tabpages,winsize
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*/.git/*
syntax enable

"" key mappings
let maplocalleader = ","
nnoremap q <nop>
nnoremap Q :q!<cr> 
inoremap jk <Esc>
cnoremap jk <C-c>
tnoremap jk <c-\><c-n>
nnoremap <silent> <C-n> :NERDTreeToggle<CR>
nnoremap <space>o :only<cr><space><bs>
nnoremap <space>w :write<cr><space><bs>
nnoremap <space><Tab> :b#<cr><space><bs>
nnoremap <space>i :BTags<cr>
nnoremap <space>g :Rg<cr>
nnoremap <space>b :Buffers<cr>
nnoremap <space>f :Files<cr>
nnoremap <space>h :History<cr>
nnoremap <C-s> :BLines<cr>
nnoremap <C-p> :GFiles<cr>
nmap f <Plug>(easymotion-bd-f)
nnoremap go <C-w>w
nnoremap gl $
nnoremap gh 0
nnoremap gm %
nnoremap <C-x><C-s> :write<cr>
nnoremap <C-x><C-k> :hide<cr>
nnoremap <C-S-f> :Rg<cr>
nnoremap <C-x>k :bd<cr>
nnoremap <C-x>g :Gstatus<cr>
nnoremap <C-g> <Esc>
inoremap <C-g> <Esc>
vnoremap <C-g> <Esc>
snoremap <C-g> <Esc>
xnoremap <C-g> <Esc>
cnoremap <C-g> <Esc>
onoremap <C-g> <Esc>
lnoremap <C-g> <Esc>
tnoremap <C-g> <Esc>
noremap <C-g> <Esc>
nnoremap <M-n> :lnext<cr> 
nnoremap <M-p> :lprev<cr> 
nnoremap <M-j> }
nnoremap <M-k> {
nnoremap <C-k> d$
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr>    pumvisible() ? "\<C-y>" : "\<cr>"

"" colors
set background=dark
colorscheme solarized8 
hi link specialchar default 
hi link special default 
hi link vertsplit normal 
hi link vimoption default
hi link delimiter default
hi link rustpreproc default
hi link rustmacro default
hi link rustmodpath default
hi link rustmodpathsep default
hi link rustoperator default
hi link rustsigil default
hi link ruststorage default
hi link rustderive default
hi link rustmacrovariable default
hi link rustattribute nontext
hi link rustassert default
hi link rustenumvariant default
hi link cocerrorsign diffdelete 
hi link cocwarningsign diffchange 
hi link cochighlighttext question 
hi link cocunderline spellbad
hi link NERDTreeFile default 
hi link clojuremacro keyword
hi link clojuredefine keyword
hi link fzf1 comment 
hi link fzf2 comment 
hi link fzf3 comment 

" fzf/skim
let g:fzf_buffers_jump = 1
let g:fzf_layout = { 'down': '~26%' }
let g:fzf_colors =
  \ { 'fg':    ['fg', 'Normal'], 'bg': ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Normal'], 'fg+': ['fg', 'Type', 'ErrorMsg', 'ErrorMsg'],
  \ 'bg+':     ['bg', 'Normal', 'Normal'],'hl+':     ['fg', 'Type'],
  \ 'info':    ['fg', 'Comment'], 'border':  ['fg', 'Comment'],
  \ 'prompt':  ['fg', 'Conditional'], 'pointer': ['fg', 'Type'],
  \ 'marker':  ['fg', 'Type'], 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

" autopairs
let g:AutoPairsShortcutToggle = ''
let g:AutoPairsShortcutJump = ''
let g:AutoPairsShortcutBackInsert = ''
let g:AutoPairsShortcutFastWrap = ''

" gitgutter
let g:gitgutter_sign_modified = '|'
let g:gitgutter_sign_added= '|'
let g:gitgutter_sign_removed= '|'
let g:gitgutter_sign_modified_removed = '|'
let g:gitgutter_override_sign_column_highlight = 1

" clojure paredit.vim 
nnoremap <M-h> :call PareditMoveLeft()<cr>
nnoremap <M-l> :call PareditMoveRight()<cr>
autocmd FileType clojure nmap <buffer> <space>e cpp
autocmd FileType clojure nmap <buffer> <space>r cpp
autocmd FileType clojure nmap <buffer> gd [<C-D> 
autocmd FileType clojure nnoremap <buffer> <C-c><C-k> :%Eval<cr>
autocmd FileType clojure nnoremap <buffer> <C-c>k :%Eval<cr>
autocmd FileType clojure nnoremap <buffer> <C-c>p :%Eval<cr><cr>:Last<cr>
autocmd FileType clojure nnoremap <buffer> <C-c><C-p> :%Eval<cr><cr>:Last<cr>

" vimrooter
let g:rooter_silent_chdir = 1

" lightline
let g:lightline = {
  \ 'mode_map': { 'n' : 'N', 'i' : 'I', 'R' : 'R', 'v' : 'V', 'V' : 'VL' },
  \ 'colorscheme': 'solarized', 'component_function': { 'gitbranch': 'fugitive#head' },
  \ 'active': {
  \ 'left': [ [ 'mode', 'paste' ], [ 'gitbranch', 'readonly', 'filename', 'modified' ] ] }, }

" nerdtree
let g:nerdtree_tabs_focus_on_files=1
let NERDTreeQuitOnOpen=1
let NERDTreeHighlightCursorline=1
let NERDTreeMouseMode=3
let NERDTreeMapActivateNode=">"
let NERDTreeMapPreview="<"
let NERDTreeMapChangeRoot="R"
let NERDTreeMapRefreshRoot="C"
let NERDTreeWinSize=25
autocmd FileType * if &ft == "nerdtree" | nnoremap go :wincmd l<cr> | nnoremap go <C-w>w | endif 
autocmd FileType nerdtree setlocal signcolumn=no
autocmd BufRead,BufNewFile * setlocal signcolumn=yes

" fugitive
autocmd FileType fugitive nnoremap <silent> <C-n> :NERDTreeToggle<cr>

" rust.vim
let g:rustfmt_autosave = 0 " coc autosaves
let g:rustfmt_emit_files = 1
let g:rustfmt_fail_silently = 0
autocmd BufReadPost *.rs setlocal filetype=rust
autocmd FileType rust nnoremap <buffer> <space>r :w<cr>:!cargo run --bin %:t:r<cr>
autocmd FileType rust nnoremap <buffer> <space>t :w<cr>:!cargo test --bin %:t:r<cr>
autocmd FileType rust nnoremap <buffer> <space>c :w<cr>:!cargo check --bin %:t:r<cr>
autocmd FileType rust set foldmethod=manual
autocmd FileType rust nnoremap za zfa} 

" coc
nmap <f2> <Plug>(coc-rename)
nmap <silent> <M-.> <Plug>(coc-diagnostic-next)
nmap <silent> <M-,> <Plug>(coc-diagnostic-prev)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
autocmd FileType rust nnoremap <silent> K :call CocAction('doHover')<cr>
autocmd CursorHold * silent call CocActionAsync('highlight')
nnoremap <silent> <space>q :exe 'CocList -I --input='.expand('<cword>').' grep'<CR>

" Remember cursor position
augroup vimrc-remember-cursor-position
  autocmd!
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
augroup END

if executable('rg')
	set grepprg=rg\ --no-heading\ --vimgrep
	set grepformat=%f:%l:%c:%m
endif

" statusline
set statusline=
set statusline+=%#CursorColumn#
set statusline+=\ %F
set statusline+=%m
set statusline+=%=
set statusline+=\ \ %l:%c
set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
set statusline+=\ %y
set statusline+=\ 
