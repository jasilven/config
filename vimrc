"" plugins
call plug#begin('~/.config/nvim/plugged')
Plug 'cormacrelf/vim-colors-github'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'kassio/neoterm'
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'cespare/vim-toml'
Plug 'easymotion/vim-easymotion'
Plug 'jiangmiao/auto-pairs', { 'tag': 'v2.0.0' }
Plug 'lifepillar/vim-solarized8'
Plug 'rust-lang/rust.vim', {'for': 'rust'}
Plug 'scrooloose/nerdtree'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-fireplace'
Plug 'guns/vim-sexp'
Plug 'venantius/vim-cljfmt'
Plug 'junegunn/fzf.vim'
call plug#end()

"" settings
set encoding=utf-8 fileencoding=utf-8 fileencodings=utf-8 spelllang=en_us
set scrolloff=2 tabstop=2 shiftwidth=2 " set t_Co=256 completeopt-=preview 
set noswapfile nobackup noshowmode nowrap noshowcmd nospell nofoldenable
set termguicolors number cursorline hidden ttyfast ignorecase hlsearch autoread
set wildmode=list:longest,full splitright
set fillchars=fold:\  listchars=tab:→\ ,trail:· nolist
set mouse=a clipboard=unnamed,unnamedplus guioptions=egmrti
set updatetime=500 undofile inccommand=nosplit
set sessionoptions=blank,curdir,help,tabpages,winsize
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*/.git/*,*/target/*
syntax enable
 
"" key mappings
let maplocalleader = ","
nmap f <Plug>(easymotion-bd-f)
nnoremap Q :q!<cr>
inoremap jk <Esc>
cnoremap jk <C-c>
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
noremap <C-g> <Esc>
nnoremap <M-n> :lnext<cr>
nnoremap <M-p> :lprev<cr>
nnoremap <M-j> }
nnoremap <M-k> {
nnoremap <C-k> d$
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr>    pumvisible() ? "\<C-y>" : "\<cr>"
tnoremap <C-w><C-w> <C-\><C-N><C-w>w
tnoremap <C-w>w <C-\><C-N><C-w>w
tnoremap jk <c-\><c-n>

"" colors
set background=dark
colorscheme solarized8
hi special guifg=#cb4b16
hi diffdelete guibg=#073642 guifg=#cb4b16
hi cursorLineNr guifg=yellow
hi VertSplit guibg=NONE
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
hi link specialchar special
hi link cocerrorsign diffdelete
hi link cocwarningsign diffchange
hi link cochighlighttext visual 
hi link cocunderline spellbad
hi link NERDTreeFile default
hi link clojuremacro keyword
hi link clojuredefine keyword
hi link fzf1 comment
hi link fzf2 comment
hi link fzf3 comment
hi link clojurekeyword identifier 
hi link clojurespecial keyword
hi easymotiontargetdefault gui=bold guifg=#EF2733

"" fzf/skim
autocmd FileType fzg tnoremap <C-e> <Esc>
let g:fzf_tags_command = 'ctags --languages=Rust,Go,Clojure,Java,JavaScript -R'
let g:fzf_buffers_jump = 1
let g:fzf_layout = { 'down': '~26%' }
let g:fzf_colors = {
  \ 'fg':      ['fg', 'Normal'], 'bg': ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Normal'], 'fg+': ['fg', 'Type', 'ErrorMsg', 'ErrorMsg'],
  \ 'bg+':     ['bg', 'Normal', 'Normal'],'hl+': ['fg', 'Type'],
  \ 'info':    ['fg', 'Comment'], 'border':  ['fg', 'Comment'],
  \ 'prompt':  ['fg', 'Conditional'], 'pointer': ['fg', 'Type'],
  \ 'marker':  ['fg', 'Type'], 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

"" autopairs
let g:AutoPairsShortcutToggle = ''
let g:AutoPairsShortcutJump = ''
let g:AutoPairsShortcutBackInsert = ''
let g:AutoPairsShortcutFastWrap = ''

"" gitgutter
let g:gitgutter_sign_modified = '|'
let g:gitgutter_sign_added= '|'
let g:gitgutter_sign_removed= '|'
let g:gitgutter_sign_modified_removed = '|'
let g:gitgutter_override_sign_column_highlight = 1

"" vimrooter
let g:rooter_silent_chdir = 1

"" lightline
let g:lightline = {
  \ 'mode_map': { 'n' : 'N', 'i' : 'I', 'R' : 'R', 'v' : 'V', 'V' : 'VL' },
  \ 'colorscheme': 'solarized', 'component_function': { 'gitbranch': 'fugitive#head' },
  \ 'active': {
  \ 'left': [ [ 'mode', 'paste' ], [ 'gitbranch', 'readonly', 'filename', 'modified' ] ] }, }

"" nerdtree
let nerdtree_tabs_focus_on_files=1
let NERDTreeQuitOnOpen=1
let NERDTreeHighlightCursorline=1
let NERDTreeMouseMode=3
let NERDTreeMapActivateNode=">"
let NERDTreeMapPreview="<"
let NERDTreeMapChangeRoot="R"
let NERDTreeMapRefreshRoot="C"
let NERDTreeWinSize=25
let NERDTreeDirArrowExpandable = ''

autocmd FileType * if &ft == "nerdtree" | nnoremap go :wincmd l<cr> | nnoremap go <C-w>w | endif
autocmd FileType nerdtree setlocal signcolumn=no
autocmd FileType nerdtree setlocal shiftwidth=1
autocmd BufRead,BufNewFile * setlocal signcolumn=yes

"" fugitive
autocmd FileType fugitive nnoremap <silent> <C-n> :NERDTreeToggle<cr>

"" rust/rust.vim
let g:rustfmt_autosave = 0 " coc autosaves
let g:rustfmt_emit_files = 0
let g:rustfmt_fail_silently = 0
autocmd BufReadPost *.rs setlocal filetype=rust
autocmd FileType rust nnoremap <silent> <buffer> <space>r :w<cr>:call CargoCmd("run", expand('%:t:r'))<cr>
autocmd FileType rust nnoremap <silent> <buffer> <space>t :w<cr>:call CargoCmd("test", expand('%:t:r'))<cr>
autocmd FileType rust nnoremap <silent> <buffer> <space>c :w<cr>:call CargoCmd("check", expand('%:t:r'))<cr>
autocmd FileType rust set foldmethod=manual
autocmd FileType rust nnoremap za zfa}
autocmd FileType rust nnoremap gz :!zeal rust: &<cr><cr>
autocmd FileType rust nnoremap <silent> K :call CocAction('doHover')<cr>
autocmd FileType rust nnoremap <silent> <f1> :call CocAction('doHover')<cr>
function! CargoCmd(cmd,bin)
	let dir = getcwd() 
	botright Topen
	Tclear
	T date
	if a:bin == "main"
		exe "T cd ". dir . " && cargo " . a:cmd 
	else
		exe "T cd ". dir . " && cargo " . a:cmd . " --bin ". a:bin
	endif
endfunction

"" coc
nmap <f2> <Plug>(coc-rename)
nmap <silent> <M-.> <Plug>(coc-diagnostic-next)
nmap <silent> <M-,> <Plug>(coc-diagnostic-prev)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> ga <Plug>(coc-codelens-action)
autocmd CursorHold * silent call CocActionAsync('highlight')
nnoremap <silent> <space>q :exe 'CocList -I --input='.expand('<cword>').' grep'<CR>
autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')

"" json
autocmd FileType json set foldmethod=manual
autocmd FileType json nnoremap za zfa}

"" sexp 
let g:sexp_mappings = { 
  \ 'sexp_capture_next_element': '<M-l>', 
  \ 'sexp_emit_tail_element': '<M-h>',
  \ 'sexp_swap_list_backward': '',
  \ 'sexp_swap_list_forward': '', }

"" clojure
autocmd FileType clojure nmap <space>e cpp
autocmd FileType clojure nmap <space>r cpp
autocmd FileType clojure nmap gd [<C-D>
autocmd FileType clojure nnoremap <C-c><C-k> :%Eval<cr>
autocmd FileType clojure nnoremap <C-c>k :%Eval<cr>
autocmd FileType clojure nnoremap <C-c>p :execute "normal! cpp\<cr>:Last\<cr>"
autocmd FileType clojure nnoremap <C-c><C-p> :execute "normal! cpp\<cr>:Last\<cr>"
autocmd FileType clojure nnoremap <silent> K :call CocAction('doHover')<cr>
autocmd FileType clojure nnoremap <silent> <f1> :call CocAction('doHover')<cr>

"" remember cursor position
augroup vimrc-remember-cursor-position
  autocmd!
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
augroup END

"" rg/grep - usage: :Grep <search> <file> 
if executable('rg')
 set grepprg=rg\ --no-heading\ --vimgrep
 set grepformat=%f:%l:%c:%m
endif
augroup quickfix
    autocmd!
    autocmd QuickFixCmdPost cgetexpr cwindow
    autocmd QuickFixCmdPost lgetexpr lwindow
augroup END
function! Grep(args)
    let args = split(a:args, ' ')
    return system(join([&grepprg, shellescape(args[0]), len(args) > 1 ? join(args[1:-1], ' ') : ''], ' '))
endfunction
command! -nargs=+ -complete=file_in_path -bar Grep  cgetexpr Grep(<q-args>)

"" statusline
set statusline=
set statusline+=%#StatusLine#
set statusline+=\ %f\ \ 
set statusline+=🗁\ %{getcwd()}\ 
set statusline+=%m
set statusline+=%=
set statusline+=%l,%.4c
set statusline+=\ %y\ %{&fileencoding?&fileencoding:&encoding}\ 

"" neoterm
let g:neoterm_size=12
let g:neoterm_autoinsert=1
autocmd BufWinEnter,WinEnter term://* startinsert
autocmd BufLeave term://* stopinsert
autocmd TermOpen * setlocal nonumber
autocmd TermOpen * startinsert
nnoremap <C-j> :botright Topen<cr>
tnoremap <C-j> <C-\><C-N><C-w><C-p>:Tclose<cr>

