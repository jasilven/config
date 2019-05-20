" =============================================================================
" # PLUGINS
" =============================================================================
call plug#begin()
Plug 'airblade/vim-gitgutter'
Plug 'tomasiser/vim-code-dark'
Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'easymotion/vim-easymotion'
" Plug 'vim-airline/vim-airline'
" Plug 'vim-airline/vim-airline-themes'
" Plug 'itchyny/lightline.vim'
Plug 'w0rp/ale'
Plug 'machakann/vim-highlightedyank'
Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'phildawes/racer'
Plug 'racer-rust/vim-racer'
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
" Completion plugins
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
" Syntactic language support
Plug 'rust-lang/rust.vim'
"Plug 'plasticboy/vim-markdown'
Plug 'rafi/awesome-vim-colorschemes'
Plug 'chriskempson/base16-vim'
call plug#end()

" =============================================================================
" # Editor settings
" =============================================================================
set noswapfile nobackup nocompatible noshowmode hidden nowrap nojoinspaces
set timeoutlen=300 encoding=utf-8 scrolloff=2 ttyfast lazyredraw synmaxcol=500
set laststatus=2 signcolumn=yes colorcolumn=80
set clipboard+=unnamed,unnamedplus mouse=a
set shell=/usr/bin/zsh
set shiftwidth=4 softtabstop=4 tabstop=4 expandtab colorcolumn=100 autoindent
set guifont=DejaVu\ Sans\ Mono:h13
set guioptions+=c guioptions-=T guioptions-=m gdefault vb t_vb= "bell
set background=dark cursorline termguicolors
set printfont=:h10 printencoding=utf-8 printoptions=paper:a4
set splitright splitbelow
set wildmenu wildmode=list:longest,list:full
set wildignore+=*.o,*.obj,.git,*.rbc,*.pyc,__pycache__,*.zip,*.jar,*~,*.png,*.jpg,*.gif,*.swp
set undodir=~/.vimdid undofile autoread
set formatoptions=tc " wrap text and comments using textwidth
set formatoptions+=r " continue comments when pressing ENTER in I mode
set formatoptions+=q " enable formatting of comments with gq
set formatoptions+=n " detect lists for formatting
set formatoptions+=b " auto-wrap in insert mode, and do not wrap old long lines
set incsearch ignorecase smartcase inccommand=nosplit
set backspace=2 ruler number
set nolist listchars=nbsp:¬,eol:¶,extends:»,precedes:«,trail:•
set noshowcmd nofoldenable foldmethod=syntax
set completeopt=noinsert,menuone,noselect
set diffopt+=iwhite " No whitespace in vimdiff
set diffopt+=algorithm:patience
set diffopt+=indent-heuristic
set shortmess+=c " don't give |ins-completion-menu| mnkessages
filetype plugin indent on
syntax on
colorscheme codedark

" =============================================================================
" # RUST and linter
" =============================================================================
let NERDTreeQuitOnOpen=1
let base16colorspace=256
let g:sneak#s_next = 1
let g:vim_markdown_new_list_item_indent = 0
let g:vim_markdown_auto_insert_bullets = 0
let g:vim_markdown_frontmatter = 1
if executable('ag')
    set grepprg=ag\ --nogroup\ --nocolor
endif
if executable('rg')
    set grepprg=rg\ --no-heading\ --vimgrep
    set grepformat=%f:%l:%c:%m
endif

" =============================================================================
" # RUST and linter
" =============================================================================
let g:ale_completion_enabled = 1
let g:ale_lint_on_enter = 1
let g:ale_lint_on_insert_leave = 1
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_linters = {'rust': ['rls']}
let g:ale_rust_rls_config = {'rust': {'all_targets': 1, 'build_on_save': 1, 'clippy_preference': 'on'}}
let g:ale_rust_rls_toolchain = ''
let g:ale_set_balloons=1
let g:ale_sign_error = "✖"
let g:ale_sign_hint = "➤"
let g:ale_sign_info = "i"
let g:ale_sign_warning = "⚠"
let g:ale_virtualtext_cursor = 1
let g:rustfmt_command = "rustfmt +stable"
let g:rustfmt_autosave = 1
let g:rustfmt_emit_files = 1
let g:rustfmt_fail_silently = 0
let g:rust_clip_command = 'xclip -selection clipboard'
let $RUST_SRC_PATH = systemlist("rustc --print sysroot")[0] . "/lib/rustlib/src/rust/src"
" hi link ALEWarningSign Todo
hi link ALEWarningSign WarningMsg
hi link ALEErrorSign ErrorMsg
hi link ALEVirtualTextWarning Todo
hi link ALEVirtualTextInfo Todo
hi link ALEVirtualTextError Todo
"hi link ALEVirtualTextError WarningMsg
hi ALEError guibg=None
hi ALEWarning guibg=None

" =============================================================================
" # FZF
" =============================================================================
let g:fzf_buffers_jump = 1
let g:fzf_layout = { 'down': '~20%' }
let $FZF_DEFAULT_COMMAND = "find * -path '*/\.*' -prune -o -path 'classes/**' -prune -o -path 'target/**' -prune -o -path 'dist/**' -prune -o -path '*.jar' -prune -o -path '*.zip' -prune -o -path '*.jpeg' -prune -o -path '*.png' -prune -o -path '*.gif' -prune -o -path '*.jpg'  -prune -o -path '*.1' -prune -o -path '*.ico'   -prune -o  -type f -print -o -type l -print 2> /dev/null"
if executable('rg')
    let $SKIM_DEFAULT_COMMAND = 'rg --files --hidden --follow --glob "!.git/*"'
    set grepprg=rg\ --vimgrep
    command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>).'| tr -d "\017"', 1, <bang>0)
endif

" =============================================================================
" # Autocommands
" =============================================================================
" Leave paste mode when leaving insert mode
autocmd InsertLeave * set nopaste
" Jump to last edit position on opening file
if has("autocmd")
    au BufReadPost * if expand('%:p') !~# '\m/\.git/' && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif
" Auto-make less files on save
autocmd BufWritePost *.less if filereadable("Makefile") | make | endif
" nvim
if has('nvim')
    runtime! plugin/python_setup.vim
endif

" =============================================================================
" # Global shortcuts
" =============================================================================
compiler cargo
:command! -nargs=* Make make <args>
":command! -nargs=* Make make <args> | cwindow
:command! -nargs=* Makeb r !make <args> | new
:command! -nargs=* -complete=shellcmd R new | setlocal buftype=nofile bufhidden=hide noswapfile | r !<args>
nmap :W :w
nmap :X :x
nmap :Q :q
nmap :E :e
map § &
map ½ ~
map å {
map ¨ }
map ¤ $
map Å [
map ^ ]
map ¼ ^
map <F1> <Esc>
map <f5> :Make check --bin %:t:r<cr>
map <f6> :Make test --bin %:t:r<cr>
map <f7> :!cargo run -q --bin %:t:r<cr>
map <f8> :Shell cargo run -q --bin %:t:r<cr>
map <f9> :nohl<cr>
nnoremap + $
inoremap jk <ESC>
inoremap kj <ESC>
tnoremap jk <C-\><C-n>
tnoremap kj <C-\><C-n>
nmap q <Esc>
map f <Plug>(easymotion-bd-f)
tnoremap <Esc> <C-\><C-n>
nnoremap j gj
nnoremap k gk
nnoremap <silent> K :ALEHover<CR>
nnoremap <silent> gd :ALEGoToDefinition<CR>
nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>
" Very magic by default
nnoremap ? ?\v
nnoremap / /\v
cnoremap %s/ %sm/
" Search results centered please
" nnoremap <silent> n nzz
" nnoremap <silent> N Nzz
" nnoremap <silent> * *zz
" nnoremap <silent> # #zz
" nnoremap <silent> g* g*zz

" =============================================================================
" # ALT/Meta shortcuts
" =============================================================================
map <M-x> <Esc>:
nmap <silent> <M-h> <Plug>(ale_previous_wrap)
nmap <silent> <M-l> <Plug>(ale_next_wrap)
nmap <M-j> :bp<cr>
tmap <M-j> :bp<cr>
tmap <M-j> :bp<cr>
nmap <M-k> :bn<cr>
tmap <M-k> :bn<cr>
tmap <M-k> :bn<cr>

" =============================================================================
" # <leader> shortcuts
" =============================================================================
let mapleader = "\<Space>"
nmap <leader>n :NERDTreeToggle<CR>
nmap <leader>c :Make check --bin %:t:r<cr>
nmap <leader>t :Make test --bin %:t:r<cr>
nmap <leader>r :!cargo run -q --bin %:t:r<cr>
nmap <leader>o :only<cr>
map <leader>f :History<CR>
map <leader>b :Buffers<CR>
nmap <leader>w :w<CR>
nmap <leader>x :bd<CR>
nmap <leader>= $
nmap <leader>a ^
nmap <leader>bd :bd<cr>
map <leader>q :bd<CR>
nnoremap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader><tab> <c-^>
nnoremap <leader><leader> <c-^>
nnoremap <leader>= :'<,'>RustFmtRange<cr>
nnoremap <leader>, :set invlist<cr>
noremap <leader>ss :Rg<space>
noremap <leader>sl :BLines<cr>
noremap <leader>sg :GFiles<cr>
" noremap <leader>p :read !xsel --clipboard --output<cr>
" noremap <leader>c :w !xsel -ib<cr><cr>

" =============================================================================
" # CTRL  (control) shortcuts
" =============================================================================

noremap <C-=> ggVG=
noremap <C-q> :confirm qall<CR>
nmap <C-n> :NERDTreeToggle<CR>
noremap <C-M-q> :x<cr>
noremap <C-f> :Rg<space>
map <C-q> :bd!<CR>
tnoremap <C-q> <C-\><C-n>:bd!<CR>
tnoremap <Esc> <C-\><C-n>:bd!<CR>
tmap <C-g> <C-\><C-n>:bd!<CR>
map <C-p> :Files<CR>
nmap <C-b> :Buffers<CR>
noremap <C-l> :BLines<cr>
inoremap <C-j> <Esc>
vnoremap <C-j> <Esc>
inoremap <C-g> <ESC>
cnoremap <C-g> <ESC>
noremap <C-g> <ESC>
noremap <C-x><C-s> :w<cr>
noremap <C-s> :w<cr>
nmap <silent> L <Plug>(ale_lint)
nmap <silent> <C-l> <Plug>(ale_detail)
nnoremap <C-.> :ALEGoToDefinition<CR>
nnoremap <silent><C-t> :call TermToggle(15)<CR>
inoremap <silent><C-t> <Esc>:call TermToggle(15)<CR>
tnoremap <silent><C-t> <C-\><C-n>:call TermToggle(12)<CR>

" =============================================================================
" # Completion shortcuts
" =============================================================================
inoremap <expr><Tab> (pumvisible()?(empty(v:completed_item)?"\<C-n>":"\<C-y>"):"\<Tab>")
inoremap <expr><CR> (pumvisible()?(empty(v:completed_item)?"\<CR>\<CR>":"\<C-y>"):"\<CR>")

" =============================================================================
" # Terminal toggle
" =============================================================================
let s:term_buf = 0
let s:term_win = 0
function! TermToggle(height)
    if win_gotoid(s:term_win)
        hide
    else
        new terminal
        exec "resize ".a:height
        try
            exec "buffer ".s:term_buf
            exec "bd terminal"
        catch
            call termopen($SHELL, {"detach": 0})
            let s:term_buf = bufnr("")
            setlocal nonu nornu scl=no nocul
        endtry
        startinsert!
        let s:term_win = win_getid()
    endif
endfunction

" =============================================================================
" # Auto commands
" =============================================================================
hi ExtraWhitespace guibg=grey
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()
autocmd FileType mail let b:delimitMate_autoclose = 0
autocmd BufEnter * call ncm2#enable_for_buffer()
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow
au FileType qf call AdjustWindowHeight(12, 15)
function! AdjustWindowHeight(minheight, maxheight)
    exe max([min([line("$"), a:maxheight]), a:minheight]) . "wincmd _"
endfunction
" Auto trim whitespace
function! TrimWhiteSpace()
    %s/\s\+$//e
endfunction
autocmd BufWritePre * :call TrimWhiteSpace()
autocmd BufWinEnter,WinEnter term://* startinsert

" =============================================================================
" # My colors
" =============================================================================
hi link rustFuncCall Normal
hi link rustFuncName Normal
hi link rustKeyword Type
hi link rustConditional Type
hi link rustRepeat Type
hi link CursorLineNr Error
hi CursorLineNr gui=underline guifg=#9CDCFE
hi CursorLineNr gui=bold guifg=#afdf00
hi GitGutterAdd    guifg=#009900
hi GitGutterChange guifg=#bbbb00
hi GitGutterDelete guifg=#ff2222
let g:gitgutter_sign_added = '|'
let g:gitgutter_sign_modified = '|'
let g:gitgutter_sign_removed = '|'
let g:gitgutter_sign_removed_first_line = '-'
let g:gitgutter_sign_modified_removed = '-'
hi Cursor guifg=white guibg=#afdf00
hi iCursor guifg=#ff2222 guibg=#ff2222
set guicursor=n-v-c:block-Cursor
set guicursor+=i:ver100-iCursor
set guicursor+=n-v-c:blinkon1

" =============================================================================
" # My statusline
" =============================================================================
au InsertEnter * hi statusline guifg=black guibg=#d7afff gui=bold
au InsertLeave * hi statusline guifg=black guibg=#8fbfdc gui=bold
hi statusline guifg=black guibg=#8fbfdc
hi User1 guibg=#2e2e2e guifg=#adadad gui=bold
set laststatus=2
set noshowmode
set statusline=
set statusline+=%0*\ %{toupper(mode())}\                 " Mode
set statusline+=%1*\ %<%F%m%r%h%w\                       " File
set statusline+=%=                                       " Right Side
set statusline+=%{FugitiveStatusline()}\ \                " git status
set statusline+=%1*%Y\ \ %{''.(&fenc!=''?&fenc:&enc).''} " Separator
set statusline+=%1*\ \ %02v/%02l\ \                      " Line number / total lines

" =============================================================================
" # :Shell command with output to new buffer
" =============================================================================
command! -complete=shellcmd -nargs=+ Shell call s:RunShellCommand(<q-args>)
function! s:RunShellCommand(cmdline)
    echo a:cmdline
    let expanded_cmdline = a:cmdline
    for part in split(a:cmdline, ' ')
        if part[0] =~ '\v[%#<]'
            let expanded_part = fnameescape(expand(part))
            let expanded_cmdline = substitute(expanded_cmdline, part, expanded_part, '')
        endif
    endfor
    botright new
    setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap
    " call setline(1, 'You entered:    ' . a:cmdline)
    " call setline(1, 'Command:  ' .expanded_cmdline)
    " call setline(2,substitute(getline(1),'.','=','g'))
    execute ':silent $read !'. expanded_cmdline
    setlocal nomodifiable
    1
endfunction
