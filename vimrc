" =============================================================================
" # PLUGINS
" =============================================================================
call plug#begin()
Plug 'tpope/vim-surround'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-gitgutter'
Plug 'tomasiser/vim-code-dark'
Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'easymotion/vim-easymotion'
Plug 'machakann/vim-highlightedyank'
Plug 'airblade/vim-rooter'
Plug 'ap/vim-buftabline'
Plug 'w0rp/ale'
Plug 'racer-rust/vim-racer'
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
call plug#end()

" =============================================================================
" # Editor settings
" =============================================================================
set undofile noswapfile nobackup noshowmode hidden wrap nojoinspaces
set timeoutlen=700 scrolloff=2 ttyfast lazyredraw synmaxcol=500
set laststatus=2 signcolumn=yes colorcolumn=80
set clipboard=unnamed,unnamedplus mouse=a
set shell=/bin/zsh
set shiftwidth=4 softtabstop=4 tabstop=4 expandtab colorcolumn=100
set guioptions+=c guioptions-=T guioptions+=a  guioptions-=m gdefault "vb t_vb= "bell
set splitright splitbelow wildignorecase
set wildmode=longest:list,full
set wildignore+=*.o,*.obj,.git,*.rbc,*.pyc,.directory,target,*.zip,*.jar,*~,*.png,*.jpg,*.gif,*.swp
set formatoptions=tcrqnb
set ignorecase smartcase inccommand=nosplit
set nolist listchars=nbsp:¬,eol:¶,extends:»,precedes:«,trail:•
set noshowcmd nofoldenable foldmethod=indent
set completeopt=noinsert,menuone,noselect
set diffopt+=iwhite " No whitespace in vimdiff
set diffopt+=algorithm:patience
set diffopt+=indent-heuristic
set shortmess+=c
set shortmess-=F
set grepprg=rg\ --no-heading\ --vimgrep
set grepformat=%f:%l:%c:%m
set cursorline termguicolors number
set autoindent autoread
" guifont=DejaVu\ Sans\ Mono:h13
filetype plugin indent on
syntax on
colorscheme codedark

" =============================================================================
" # Misc
" =============================================================================
let g:buftabline_indicators = 1
let g:rooter_silent_chdir = 1
let g:NERDTreeWinSize=23
let NERDTreeQuitOnOpen=1
let base16colorspace=256
let g:sneak#s_next = 1
let g:vim_markdown_new_list_item_indent = 0
let g:vim_markdown_auto_insert_bullets = 0
let g:vim_markdown_frontmatter = 1
:command! -nargs=* -complete=shellcmd R new | setlocal buftype=nofile bufhidden=hide noswapfile | r !<args>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" =============================================================================
" # RUST
" =============================================================================
" let g:racer_experimental_completer = 1
" let g:racer_insert_paren = 1
let g:ale_sign_error = '×'
let g:ale_sign_warning = '⚠'
let g:ale_lint_on_text_changed = 'always'
let g:ale_lint_on_save = 1
let g:ale_cursor_detail= 0
let g:ale_virtualtext_cursor = 0
let g:ale_virtualtext_prefix = '»'
let g:ale_linters = {'rust': ['rls']}
let g:ale_fixers = {'XXXXX': ['remove_trailing_lines', 'trim_whitespace'],'rust': ['rustfmt'],}
let g:ale_fix_on_save = 0
let g:ale_rust_cargo_use_check = 1
let g:ale_completion_enabled = 1
let g:ale_rust_rls_toolchain = "stable"
set omnifunc=ale#completion#OmniFunc
let g:rustfmt_autosave = 1
au FileType rust noremap gd :ALEGoToDefinition<CR>
au FileType rust map <F1> <Plug>(rust-doc)
au FileType rust noremap <leader>i :ALEDetail<cr><C-w>k
" au FileType rust nmap K <Plug>(rust-doc)
au FileType rust nmap K :ALEHover<cr>
" au FileType rust set makeprg=cargo\ check\ --bin\ %:t:r
au FileType rust set makeprg=cargo
au FileType rust nmap <leader>c :w<cr>:silent :make check<cr>
au FileType rust nmap <leader>t :w<cr>:!cargo test --bin %:t:r<cr>
" au FileType rust nmap <leader>r :w<cr>:!time RUST_BACKTRACE=1 cargo run -q --bin %:t:r<cr>
au FileType rust nmap <leader>r :w<cr>:!time cargo run -q --bin %:t:r<cr>
au FileType rust nmap <f5> :w<cr>:silent :make<cr>
au FileType rust nmap <f6> :w<cr>:!cargo test --bin %:t:r<cr>
au FileType rust nmap <f8> :w<cr>:!RUST_BACKTRACE=1 cargo run -q --bin %:t:r<cr>
au FileType rust map <M-j> :cp<cr>
au FileType rust map <M-k> :cn<cr>
au FileType rust :iabbrev prn println!("{}",&);<ESC>2h
au FileType rust :iabbrev dbg dbg!(&);<ESC>2h

" =============================================================================
" # FZF
" =============================================================================
let g:fzf_tags_command = 'ctags -R --exclude=.git --exclude=.vscode --exclude=target'
let g:fzf_buffers_jump = 1
let g:fzf_layout = { 'down': '~20%' }
let $FZF_DEFAULT_COMMAND = "find * -path '*/\.*' -path 'tags' -prune -o -path '.rustup' -prune -o -path 'classes' -prune -o -path 'target' -prune -o -path 'dist' -prune -o -path '*.jar' -prune -o -path '*.zip' -prune -o -path '*.jpeg' -prune -o -path '*.png' -prune -o -path '*.gif' -prune -o -path '*.jpg'  -prune -o -path '*.1' -prune -o -path '*.ico' -prune -o  -type f -print -o -type l -print 2> /dev/null"
let $SKIM_DEFAULT_COMMAND = 'rg --files --hidden --follow --glob "!.git/*"'
set grepprg=rg\ --vimgrep
command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>).'| tr -d "\017"', 1, <bang>0)
autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
            \| autocmd BufLeave <buffer> set laststatus=2 noshowmode ruler
let g:fzf_colors =
            \ { 'fg':    ['fg', 'Normal'],
            \ 'bg':      ['bg', 'Normal'],
            \ 'hl':      ['fg', 'WildMenu'],
            \ 'fg+':     ['fg', 'Normal', 'CursorLine', 'CursorColumn', 'Normal'],
            \ 'bg+':     ['bg', 'WildMenu', ],
            \ 'hl+':     ['fg', 'ErrorMsg'],
            \ 'info':    ['fg', 'PreProc'],
            \ 'border':  ['fg', 'Normal'],
            \ 'prompt':  ['fg', 'Conditional'],
            \ 'pointer': ['fg', 'ErrorMsg'],
            \ 'marker':  ['fg', 'Normal'],
            \ 'spinner': ['fg', 'Label'],
            \ 'header':  ['fg', 'Comment'] }

" =============================================================================
" # Global shortcuts
" =============================================================================
nmap <TAB> :bn<cr>
nmap <S-TAB> :bp<cr>
map <F2> :%s/\<<C-r><C-w>\>//c<left><left>
nmap :Q :q
nmap ; <nop>
map <f9> :nohl<CR>
nnoremap d4 d$
nnoremap y4 y$
inoremap jk <ESC>
inoremap kj <ESC>
tnoremap jk <C-\><C-n>
tnoremap kj <C-\><C-n>
nmap q <Esc>
nmap f <Plug>(easymotion-bd-f)
tnoremap <Esc> <C-\><C-n>
nnoremap j gj
nnoremap k gk

" =============================================================================
" # ALT/Meta shortcuts
" =============================================================================
noremap <M-x> :Commands<CR>
inoremap <M-x> <esc>:Commands<CR>
noremap <C-p> :Commands<CR>
inoremap <C-p> <esc>:Commands<CR>

" =============================================================================
" # <leader> shortcuts
" =============================================================================
let mapleader = "\<Space>"
nmap <leader>n :NERDTreeToggle<CR>
nmap <leader>o :only<CR>
map <leader>f :Files<CR>
map <leader>h :History<CR>
map <leader>j :BTags<CR>
map <leader>b :Buffers<CR>
map <leader>g :Rg<space><C-r><C-w>
nmap <leader>w :w<CR>
nnoremap <leader>, :set invlist<cr>

" =============================================================================
" # CTRL  (control) shortcuts
" =============================================================================
noremap <C-q> :confirm qall<CR>
inoremap <C-q> <esc>:bd!<CR>
tnoremap <C-q> <C-\><C-n>:bd!<CR>
noremap <C-q> :bd!<CR>
map <C-M-q> :x!<cr>
tmap <C-g> <C-\><C-n>:bd!<CR>
tnoremap <C-g> <C-\><C-n>:bd!<CR>
inoremap <C-g> <ESC>
nmap <C-n> :NERDTreeToggle<CR>
noremap <C-f> :BLines<space><cr>
tnoremap <Esc> <C-\><C-n>:bd!<CR>
noremap <C-j> 10j
noremap <C-k> 10k
noremap <C-x><C-s> :w<CR>
inoremap <C-s> <esc>:w<CR>
noremap <C-s> :w<CR>
nnoremap <silent><C-t> :call TermToggle(15)<CR>
inoremap <silent><C-t> <Esc>:call TermToggle(15)<CR>
tnoremap <silent><C-t> <C-\><C-n>:call TermToggle(15)<CR>

" =============================================================================
" # Completion shortcuts
" =============================================================================
inoremap <expr><TAB> (pumvisible()?"\<C-n>":"\<Tab>")
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
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow
au FileType qf call AdjustWindowHeight(15, 20)
function! AdjustWindowHeight(minheight, maxheight)
    exe max([min([line("$"), a:maxheight]), a:minheight]) . "wincmd _"
endfunction
function! TrimWhiteSpace()
    %s/\s\+$//e
endfunction
autocmd BufWritePre * :call TrimWhiteSpace()
autocmd BufWinEnter,WinEnter term://* startinsert
autocmd InsertLeave * set nopaste
if has("autocmd")
    au BufReadPost * if expand('%:p') !~# '\m/\.git/' && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif
runtime! plugin/python_setup.vim
autocmd FileType netrw setl bufhidden=wipe " allow netrw buffer close

" =============================================================================
" # My colors
" =============================================================================
hi link BufTabLineCurrent Title
hi link BufTabLineActive SignColumn
hi link BufTabLineHidden SignColumn
hi fzf1 guifg=gray
hi CursorLine guibg=#2c2c2c
hi CursorLineNr gui=underline guifg=#9CDCFE
hi CursorLineNr gui=bold guifg=#afdf00
hi Search guifg=yellow
hi IncSearch guifg=yellow gui=bold
hi ALEWarning gui=undercurl guisp=#608B4E
hi ALEError gui=undercurl guisp=#608B4E
hi ALEErrorSign guifg=#F44747 gui=bold
hi ALEWarningSign guifg=yellow
hi link QuickFixLine WildMenu
hi link rustFuncCall Keyword
hi link rustMacro NonText
hi link rustDerive NonText
hi link rustMacroVariable NonText
hi link rustMacro Normal
hi link rustAttribute NonText
hi link rustFuncName Keyword
hi link rustKeyword Type
hi link rustConditional Type
hi link rustRepeat Type
hi link CursorLineNr Error
hi GitGutterAdd    guifg=#009900
hi GitGutterChange guifg=#bbbb00
hi GitGutterDelete guifg=#ff2222
let g:gitgutter_sign_added = '|'
let g:gitgutter_sign_modified = '|'
let g:gitgutter_sign_removed = '|'
let g:gitgutter_sign_removed_first_line = '-'
let g:gitgutter_sign_modified_removed = '-'
" hi Cursor guifg=#000000 guibg=#afdf00
hi iCursor guifg=#000000 guibg=#ff2222
set guicursor=n-v-c:block-Cursor
set guicursor+=i:ver75-iCursor
set guicursor+=n-v-c:blinkon1

" =============================================================================
" # My statusline
" =============================================================================
hi statusline guifg=black guibg=#bbbbbb gui=bold
hi statuslineNC guifg=#444444 guibg=#777777
hi User1 guibg=#bbbbbb guifg=black gui=bold
hi User2 guibg=#bbbbbb guifg=#444444 gui=bold
set noshowmode
set statusline=
set statusline+=%0*\ %{toupper(mode())}\                 " Mode
set statusline+=%1*\ %<%F%m%r%h%w\                       " File
set statusline+=%=                                       " Right Side
set statusline+=%2*%{FugitiveStatusline()}\ \                " git status
set statusline+=%2*%Y\ \ %{''.(&fenc!=''?&fenc:&enc).''} " Separator
set statusline+=%2*\ \ %02v:%02l\ \                      " Line number / total lines

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
    setlocal noswapfile nowrap
    execute ':silent $read !'. expanded_cmdline
    setlocal modifiable
    1
endfunction
