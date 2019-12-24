"" plugins
call plug#begin('~/.config/nvim/plugged')
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'liuchengxu/vista.vim'
Plug 'kovisoft/paredit'
Plug 'benmills/vimux'
" Plug 'neomake/neomake'
Plug 'ludovicchabant/vim-gutentags'
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'cespare/vim-toml'
Plug 'easymotion/vim-easymotion'
Plug 'itchyny/lightline.vim'
Plug 'jiangmiao/auto-pairs', { 'tag': 'v2.0.0' }
Plug 'joshdick/onedark.vim'
Plug 'lifepillar/vim-solarized8'
Plug 'liuchengxu/vim-clap', { 'do': function('clap#helper#build_all') }
Plug 'rust-lang/rust.vim', {'for': 'rust'}
Plug 'scrooloose/nerdtree'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-fireplace'
Plug 'venantius/vim-cljfmt'
call plug#end()

"" settings
set encoding=utf-8 fileencoding=utf-8 fileencodings=utf-8 spelllang=en_us
set scrolloff=2 tabstop=2 shiftwidth=2 " set t_Co=256
" set completeopt-=preview 
set nolist norelativenumber nospell noswapfile nobackup noshowmode nowrap noshowcmd nospell 
set termguicolors number cursorline hidden ttyfast ruler ignorecase hlsearch 
set wildmode=list:longest,full 
set mouse=a clipboard=unnamed,unnamedplus guioptions=egmrti
set updatetime=500
set sessionoptions=blank,curdir,help,tabpages,winsize
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*/.git/*
set background=dark
syntax enable
colorscheme solarized8

"" key mappings
let maplocalleader = ","
nnoremap <silent> <C-l> :nohl<CR><C-l>
nnoremap q <nop>
nnoremap Q :q!<cr> 
inoremap jk <Esc>
cnoremap jk <C-c>
tnoremap jk <c-\><c-n>
nnoremap P :Clap yanks<cr>
nnoremap <silent> <C-n> :NERDTreeToggle<CR>
nnoremap <space>* :Clap grep ++query=<cword><cr><space><bs>
nnoremap <space>o :only<cr><space><bs>
nnoremap <space>i :Clap tags<cr><space><bs>
nnoremap <space>w :write<cr><space><bs>
nnoremap <space>g :Clap grep<cr><space><bs>
nnoremap <space>b :Clap buffers<cr><space><bs>
nnoremap <space>f :Clap files --hidden<cr>
nnoremap <space>h :Clap history<cr><space><bs>
nnoremap <space><Tab> :b#<cr><space><bs>
nnoremap <C-s> :Clap blines<cr><space><bs>
nnoremap <C-p> :Clap git_files<cr><space><bs>
nmap f <Plug>(easymotion-bd-f)
nnoremap go <C-w>w
nnoremap gl $
nnoremap gh 0
nnoremap gm %
nnoremap <C-x><C-s> :write<cr>
nnoremap <C-x><C-k> :hide<cr>
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
nnoremap <M-j> }
nnoremap <M-k> {
nnoremap <C-k> d$
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr>    pumvisible() ? "\<C-y>" : "\<cr>"

"" colors
hi signcolumn guibg=#002b36
hi linenr guibg=#002b36
hi vertsplit guibg=#002b36 guifg=#586E75
hi special ctermfg=13 guifg=#2aa198
hi NeomakeWarningSign guifg=#e5d11c 
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
hi link cocerrorsign warningmsg
hi link cochighlighttext search 
hi link NERDTreeFile default 
hi link clojuremacro keyword
hi link clojuredefine keyword

"" gitgutter
let g:gitgutter_sign_modified = '|'
let g:gitgutter_sign_added= '|'
let g:gitgutter_sign_removed= '|'
let g:gitgutter_sign_modified_removed = '|'
let g:gitgutter_override_sign_column_highlight = 0

"" clojure paredit.vim 
nnoremap <M-h> :call PareditMoveLeft()<cr>
nnoremap <M-l> :call PareditMoveRight()<cr>
autocmd FileType clojure nmap <buffer> <space>e cpp
autocmd FileType clojure nmap <buffer> <space>r cpp
autocmd FileType clojure nmap <buffer> gd [<C-D> 
autocmd FileType clojure nnoremap <buffer> <C-c><C-k> :%Eval<cr>
autocmd FileType clojure nnoremap <buffer> <C-c>k :%Eval<cr>
autocmd FileType clojure nnoremap <buffer> <C-c>p :%Eval<cr><cr>:Last<cr>
autocmd FileType clojure nnoremap <buffer> <C-c><C-p> :%Eval<cr><cr>:Last<cr>

"" neomake
" call neomake#configure#automake({'TextChanged': {},'InsertLeave': {},'BufWritePost': {'delay': 0},'BufWinEnter': {},}, 500)
" let g:neomake_warning_sign = { 'text': '✖' ,'texthl': 'NeomakeWarningSign', }

"" vimrooter
let g:rooter_silent_chdir = 1

"" lightline
let g:lightline = {
      \ 'colorscheme': 'solarized', 'component_function': { 'gitbranch': 'fugitive#head' },
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'gitbranch', 'readonly', 'filename', 'modified' ] ] }, }

" nerdtree
let g:nerdtree_tabs_focus_on_files=1
let NERDTreeQuitOnOpen=1
let NERDTreeHighlightCursorline=1
let NERDTreeMouseMode=3
let NERDTreeMapActivateNode=">"
let NERDTreeMapPreview="<"
let NERDTreeMapChangeRoot="R"
let NERDTreeMapRefreshRoot="C"
autocmd FileType * if &ft == "nerdtree" | nnoremap go :wincmd l<cr> | nnoremap go <C-w>w | endif 
autocmd FileType nerdtree setlocal signcolumn=no
autocmd BufRead,BufNewFile * setlocal signcolumn=yes

" fugitive
autocmd FileType fugitive nnoremap <silent> <C-n> :NERDTreeToggle<cr>

" rust.vim
let g:rustfmt_autosave = 1
let g:rustfmt_emit_files = 1
let g:rustfmt_fail_silently = 0
autocmd BufReadPost *.rs setlocal filetype=rust
autocmd FileType rust nnoremap <buffer> <space>r :w<cr>:!cargo run --bin %:t:r<cr>
autocmd FileType rust nnoremap <buffer> <space>t :w<cr>:!cargo test --bin %:t:r<cr>
autocmd FileType rust nnoremap <buffer> <space>c :w<cr>:!cargo check --bin %:t:r<cr>
autocmd FileType rust set foldmethod=manual
autocmd FileType rust nnoremap za zfa} 

"" coc
nmap <f2> <Plug>(coc-rename)
nmap <silent> <M-.> <Plug>(coc-diagnostic-next)
nmap <silent> <M-,> <Plug>(coc-diagnostic-prev)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
autocmd FileType rust nnoremap <silent> K :call CocAction('doHover')<cr>
" nnoremap FileType rust <silent> K :call <SID>show_documentation()<CR>
" function! s:show_documentation()
"   if (index(['vim','help'], &filetype) >= 0)
"     execute 'h '.expand('<cword>')
"   else
"     call CocAction('doHover')
"   endif
" endfunction
" autocmd CursorHold * silent call CocActionAsync('highlight')

"" Remember cursor position
augroup vimrc-remember-cursor-position
  autocmd!
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
augroup END

if executable('rg')
	set grepprg=rg\ --no-heading\ --vimgrep
	set grepformat=%f:%l:%c:%m
endif
