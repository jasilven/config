"### PLUGINS
call plug#begin()
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
Plug 'itchyny/lightline.vim'
Plug 'chriskempson/base16-vim'
Plug 'flazz/vim-colorschemes'
Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-salve'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'guns/vim-sexp', {'for': 'clojure'}
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf'
Plug 'airblade/vim-gitgutter'
Plug 'easymotion/vim-easymotion'
Plug 'tomasiser/vim-code-dark'
Plug 'Raimondi/delimitMate'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'machakann/vim-highlightedyank'
Plug 'airblade/vim-rooter'
Plug 'w0rp/ale'
Plug 'racer-rust/vim-racer'
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'venantius/vim-cljfmt'
Plug 'guns/vim-clojure-static'
Plug 'guns/vim-clojure-highlight'
Plug 'tpope/vim-dispatch'
Plug 'clojure-vim/vim-jack-in'
Plug 'radenling/vim-dispatch-neovim'
call plug#end()

"### EDITOR SETTINGS
set undofile noswapfile nobackup hidden wrap nojoinspaces number
set laststatus=2 shell=/bin/zsh clipboard=unnamed,unnamedplus mouse=a
set splitright splitbelow incsearch ignorecase smartcase signcolumn=yes
set shiftwidth=4 softtabstop=4 tabstop=4 expandtab
set background=dark cursorline termguicolors
set completeopt=noinsert,menuone,noselect
filetype plugin indent on
syntax on
colorscheme base16-tomorrow-night
set guicursor=n-v-c:block-Cursor
set guicursor+=i:ver75-iCursor
set guicursor+=n-v-c:blinkon1
" hi Normal guibg=#000000
" hi Type guifg=#ebdbb2
" hi link Type Normal

"### MISC
set grepprg=rg\ --no-heading\ --vimgrep
set grepformat=%f:%l:%c:%m
let g:airline#extensions#tabline#enabled = 1
let g:rooter_silent_chdir = 1
let g:NERDTreeWinSize=25
let NERDTreeQuitOnOpen=1
let g:sneak#s_next = 1
let g:vim_markdown_new_list_item_indent = 0
let g:vim_markdown_auto_insert_bullets = 0
let g:vim_markdown_frontmatter = 1
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" ### COMPLETION
autocmd BufEnter * call ncm2#enable_for_buffer()
au User Ncm2Plugin call ncm2#register_source({
        \ 'name' : 'css',
        \ 'priority': 9,
        \ 'subscope_enable': 1,
        \ 'scope': ['css','scss'],
        \ 'mark': 'css',
        \ 'word_pattern': '[\w\-]+',
        \ 'complete_pattern': ':\s*',
        \ 'on_complete': ['ncm2#on_complete#omni', 'csscomplete#CompleteCSS'],
        \ })
" set omnifunc=ale#completion#OmniFunc

"### ALE
let g:ale_completion_enabled = 1
let g:ale_cursor_detail= 0
let g:ale_fix_on_save = 0
let g:ale_lint_on_insert_leave = 1
let g:ale_lint_on_save = 0
let g:ale_lint_on_text_changed = 'never'
let g:ale_sign_error = '×'
let g:ale_sign_warning = '⚠'
let g:ale_virtualtext_cursor = 1
let g:ale_virtualtext_prefix = '  » '

"### CLOJURE
let g:clj_fmt_autosave = 1
let g:ale_linters = {'clojure': ['joker']}
au Filetype clojure nnoremap <c-c><c-k> :Require<cr>
au Filetype clojure nnoremap <C-x><C-x> :Eval<cr>
au FileType clojure nnoremap gd :Djump<space><C-r><C-w><cr>
au Filetype clojure nmap <buffer> <M-h> <Plug>(sexp_emit_tail_element)
au Filetype clojure nmap <buffer> <M-l> <Plug>(sexp_capture_next_element)

" ### RUST
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'rust': ['rustfmt'],
\}
let g:rustfmt_fail_silently = 0
let g:ale_rust_rls_toolchain = "stable"
let g:rustfmt_autosave = 1
let g:ale_linters = {'rust': ['rls']}
let g:ale_rust_cargo_use_check = 1
let g:ale_rust_rls_config = {'rust': {'all_targets': 1,'build_on_save': 1,'clippy_preference': 'on'}}
au FileType rust nnoremap <f5> :w<cr>:silent :make<cr>
au FileType rust nnoremap <f6> :w<cr>:!cargo test --bin %:t:r<cr>
au FileType rust nnoremap <f7> :w<cr>:!RUST_BACKTRACE=1 cargo run -q --bin %:t:r<cr>
au FileType rust nnoremap <silent> K :ALEHover<CR>
au FileType rust set makeprg=cargo
" ### GLOBAL SHORTCUTS
nnoremap :Q :q
nmap ; <nop>
nnoremap gh ^
nnoremap gl $
inoremap jk <ESC>
inoremap kj <ESC>
tnoremap jk <C-\><C-n>
tnoremap kj <C-\><C-n>
nmap q <Esc>
nmap f <Plug>(easymotion-bd-f)
tnoremap <Esc> <C-\><C-n>

"### ALT,META SHORTCUTS
nnoremap <M-x> :Commands<CR>
inoremap <M-x> <esc>:Commands<CR>

"### LEADER SHORTCUTS
let mapleader = "\<Space>"
nnoremap <leader>o :only<CR>
nnoremap <leader>f :Files<CR>
nnoremap <leader>h :History<CR>
nnoremap <leader>j :BTags<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>l :BLines<CR>
nnoremap <leader>g :Rg<space>
nnoremap <leader>w :w<CR>
nnoremap <leader>, :set invlist<cr>

"### CTRL SHORTCUTS
nnoremap <C-p> :GFiles<CR>
nnoremap <C-n> :NERDTreeToggle<CR>
nnoremap <C-f> :BLines<space><cr>
nnoremap <C-F> :Rg<space><cr>
nnoremap <C-c> <nop>
cnoremap <C-g> <esc><CR>
inoremap <C-q> <esc>:bd!<CR>
tnoremap <C-q> <C-\><C-n>:bd!<CR>
nnoremap <C-q> :bd!<CR>
tnoremap <C-g> <C-\><C-n>:bd!<CR>
tnoremap <C-g> <C-\><C-n>:bd!<CR>
inoremap <C-g> <ESC>
tnoremap <Esc> <C-\><C-n>:bd!<CR>
nnoremap <C-j> 10j
nnoremap <C-k> 10k
nnoremap <C-x><C-s> :w<CR>
inoremap <C-s> <esc>:w<CR>
nnoremap <C-s> :w<CR>

"### Completion
inoremap <expr><TAB> (pumvisible()?"\<C-n>":"\<Tab>")
inoremap <expr><CR> (pumvisible()?(empty(v:completed_item)?"\<CR>\<CR>":"\<C-y>"):"\<CR>")

" ### My colors
" hi link BufTabLineCurrent DiffText
" hi link BufTabLineActive SignColumn
" hi link BufTabLineHidden SignColumn
" hi fzf1 guifg=gray
" hi CursorLine guibg=#2c2c2c
" hi CursorLineNr gui=underline guifg=#9CDCFE
" hi CursorLineNr gui=bold guifg=#afdf00
" hi Search guifg=yellow
" hi IncSearch guifg=yellow gui=bold
" hi link ALEVirtualTextError vimCommentTitle
" hi link ALEVirtualTextWarning vimCommentTitle
" " hi ALEWarning gui=undercurl guisp=#608B4E
" " hi ALEError gui=undercurl guisp=#608B4E
" " hi ALEErrorSign guifg=#F44747 gui=bold
" " hi ALEWarningSign guifg=yellow
" hi link QuickFixLine WildMenu
" hi link rustFuncCall Keyword
" hi link rustMacro NonText
" hi link rustDerive NonText
" hi link rustMacroVariable NonText
" hi link rustMacro Normal
" hi link rustAttribute NonText
" hi link rustFuncName Keyword
" hi link rustKeyword Type
" hi link rustConditional Type
" hi link rustRepeat Type
" hi link CursorLineNr Error
" hi link GitGutterAdd SignColumn
" hi link GitGutterChange SignColumn
" hi link GitGutterDelete SignColumn
" let g:gitgutter_sign_added = '|'
" let g:gitgutter_sign_modified = '“'
" let g:gitgutter_sign_removed = '•'
" let g:gitgutter_sign_removed_first_line = '-'
" let g:gitgutter_sign_modified_removed = '-'
" " hi Cursor guifg=#000000 guibg=#afdf00
" hi iCursor guifg=#000000 guibg=#ff2222

" set formatoptions+=a
" set guioptions+=c guioptions-=T guioptions+=a guioptions-=m gdefault "vb t_vb= "bell
" set timeoutlen=700 scrolloff=2 ttyfast lazyredraw synmaxcol=500
" set laststatus=2 signcolumn=yes colorcolumn=80
" set splitright splitbelow wildignorecase
" set wildmode=longest:list,full
" set wildignore+=*.o,*.obj,.git,*.rbc,*.pyc,.directory,target,*.zip,*.jar,*~,*.png,*.jpg,*.gif,*.swp
" set wildignore+=.git,classes
" set formatoptions=tcrqnb
" set ignorecase smartcase inccommand=nosplit
" set nolist listchars=nbsp:¬,eol:¶,extends:»,precedes:«,trail:•
" set noshowcmd nofoldenable foldmethod=syntax
" set diffopt+=iwhite " No whitespace in vimdiff
" set diffopt+=algorithm:patience
" set diffopt+=indent-heuristic
" set shortmess+=c
" set shortmess-=F
" set grepprg=rg\ --no-heading\ --vimgrep
" set grepformat=%f:%l:%c:%m
" set cursorline termguicolors number
" set autoindent autoread nohlsearch
" guifont=DejaVu\ Sans\ Mono:h13 let g:gruvbox_contrast_dark = 'medium'
"
" au Filetype clojure nmap <C-x><C-e> cpp<cr>
" au Filetype clojure nmap <C-c><C-c> cpp<cr>
" au FileType clojure nmap <leader>e <Plug>(ale_next_wrap)<C-r><C-w>
" au Filetype clojure nmap <buffer> <C-left> <Plug>(sexp_emit_tail_element)
" au Filetype clojure nmap <buffer> <C-right> <Plug>(sexp_capture_next_element)
" =============================================================================
" # ALE
" =============================================================================
" " let g:racer_experimental_completer = 1
" " let g:racer_insert_paren = 1
" let g:ale_sign_error = '×'
" let g:ale_sign_warning = '⚠'
" let g:ale_lint_on_text_changed = 'never'
" let g:ale_lint_on_insert_leave = 1
" let g:ale_lint_on_save = 1
" let g:ale_cursor_detail= 0
" let g:ale_virtualtext_cursor = 1
" let g:ale_virtualtext_prefix = '  » '
" let g:ale_linters = {'rust': ['rls']}
" let g:ale_linters = {'clojure': ['joker']}
" let g:ale_fixers = {'XXXXX': ['remove_trailing_lines', 'trim_whitespace'],'rust': ['rustfmt'],}
" let g:ale_fix_on_save = 0
" let g:ale_rust_cargo_use_check = 1
" let g:ale_completion_enabled = 0
" " set omnifunc=ale#completion#OmniFunc
" let g:ale_rust_rls_toolchain = "stable"
" let g:ale_rust_rls_config = {'rust': {'all_targets': 1,'build_on_save': 1,'clippy_preference': 'on'}}
" =============================================================================
" # FZF
" =============================================================================
" let g:fzf_tags_command = 'ctags -R --exclude=.git --exclude=.vscode --exclude=target'
" let g:fzf_buffers_jump = 1
" let g:fzf_layout = { 'down': '~20%' }
" let $FZF_DEFAULT_COMMAND = "find * -path '*/\.*' -path 'tags' -prune -o -path '.rustup' -prune -o -path 'classes' -prune -o -path 'target' -prune -o -path 'dist' -prune -o -path '*.jar' -prune -o -path '*.zip' -prune -o -path '*.jpeg' -prune -o -path '*.png' -prune -o -path '*.gif' -prune -o -path '*.jpg'  -prune -o -path '*.1' -prune -o -path '*.ico' -prune -o  -type f -print -o -type l -print 2> /dev/null"
" let $SKIM_DEFAULT_COMMAND = 'rg --files --hidden --follow --glob "!.git/*"'
" set grepprg=rg\ --vimgrep
" command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>).'| tr -d "\017"', 1, <bang>0)
" autocmd! FileType fzf
" autocmd  FileType fzf set laststatus=0 noshowmode noruler
"             \| autocmd BufLeave <buffer> set laststatus=2 noshowmode ruler
" " let g:fzf_colors =
"             \ { 'fg':    ['fg', 'Normal'],
"             \ 'bg':      ['bg', 'Normal'],
"             \ 'hl':      ['fg', 'WildMenu'],
"             \ 'fg+':     ['fg', 'Normal', 'CursorLine', 'CursorColumn', 'Normal'],
"             \ 'bg+':     ['bg', 'WildMenu', ],
"             \ 'hl+':     ['fg', 'ErrorMsg'],
"             \ 'info':    ['fg', 'PreProc'],
"             \ 'border':  ['fg', 'Normal'],
"             \ 'prompt':  ['fg', 'Conditional'],
"             \ 'pointer': ['fg', 'ErrorMsg'],
"             \ 'marker':  ['fg', 'Normal'],
"             \ 'spinner': ['fg', 'Label'],
"             \ 'header':  ['fg', 'Comment'] }
