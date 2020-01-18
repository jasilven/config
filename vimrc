"" plugins
call plug#begin('~/.config/nvim/plugged')
Plug 'mbbill/undotree'
Plug 'lifepillar/vim-solarized8'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'ludovicchabant/vim-gutentags'
Plug 'kassio/neoterm'
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'cespare/vim-toml'
Plug 'easymotion/vim-easymotion'
Plug 'jiangmiao/auto-pairs', { 'tag': 'v2.0.0' }
Plug 'rust-lang/rust.vim', {'for': 'rust'}
Plug 'scrooloose/nerdtree'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-vinegar'
Plug 'guns/vim-sexp'
Plug 'venantius/vim-cljfmt'
Plug 'junegunn/fzf.vim'
Plug 'lotabout/fzf'
" Plug 'lotabout/skim.vim'
" Plug 'lotabout/skim'
call plug#end()

"" settings
set encoding=utf-8 fileencoding=utf-8 fileencodings=utf-8 spelllang=en_us
set scrolloff=2 tabstop=2 shiftwidth=2 " set t_Co=256 completeopt-=preview 
set noswapfile nobackup noshowmode nowrap noshowcmd nospell nofoldenable
set termguicolors number cursorline hidden ttyfast ignorecase hlsearch autoread
set wildmode=list:longest,full splitright
" set fillchars=fold:\ ,stlnc:\ ,vert:│  nolist
set fillchars=fold:\ ,vert:│  nolist
set listchars=tab:→\ ,space:·,nbsp:␣,trail:•,eol:$,precedes:«,extends:»
set mouse=a clipboard=unnamed,unnamedplus guioptions=egmrti
set undodir=~/.undodir undofile
set updatetime=500 inccommand=nosplit
set sessionoptions=blank,curdir,help,tabpages,winsize
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*/.git/*,*/target/*,*~
au BufWinEnter,WinEnter * set cursorline 
au BufLeave * set nocursorline 
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
nnoremap dl d$
nnoremap dh d0
nnoremap gl $
nnoremap gh 0
nnoremap gm %
nnoremap <C-x><C-s> :write<cr>
nnoremap <C-x><C-k> :hide<cr>
nnoremap <C-S-f> :Rg<cr>
nnoremap <C-x>k :close<cr>
nnoremap <C-x>g :Gstatus<cr>
nnoremap <C-x>u :UndotreeToggle<cr>
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
tnoremap <C-w>k <C-\><C-N><C-w>k
tnoremap <C-w>j <C-\><C-N><C-w>j
tnoremap <C-w>h <C-\><C-N><C-w>h
tnoremap <C-w>l <C-\><C-N><C-w>l
tnoremap jk <c-\><c-n>

function! Solarized()
	syntax reset
	set background=dark
	colorscheme solarized8 
	hi! special guifg=#839496
	hi! special guifg=#b58900
  hi! VertSplit guibg=NONE
  hi! cursorLineNr guifg=white
	hi! linenr guibg=NONE guifg=#004252
	hi! underlined guifg=#6c71c4 gui=underline
  hi! link delimiter default
  hi! link preproc default
	hi! link type default
	hi! link cocerrorsign warningmsg 
	hi! link cocwarningsign conceal 
	hi! link cochighlighttext visual 
	hi! link cocunderline spellbad
	hi! link cocerrorfloat diffdelete 
	hi! link cocwarningfloat diffchange 
	hi! link clojuremacro keyword
	hi! link clojuredefine keyword
	hi! link clojurekeyword identifier 
	hi! link clojurespecial keyword
	hi! link tabline pmenu
	hi! link tablinefill pmenu
	hi! link tablinesel normal
endfunction
command! MySolarized call Solarized()

function! Solarized2()
	syntax reset
	set background=dark
	colorscheme solarized8 
	hi! normal guifg=#a3a3a3 guibg=#1A1A1A
	hi! statusline guifg=#a3a3a3 guibg=#1A1A1A 
  hi! vertsplit guibg=NONE gui=NONE guifg=#363636
	hi! signcolumn guibg=NONE
	hi! special guifg=#839496
	hi! special guifg=#b58900
  hi! VertSplit guibg=NONE
	hi! cursorline guibg=#212121 gui=NONE
	hi! cursorLineNr guifg=#BDBD00 guibg=NONE
	hi! linenr guifg=#303030 guibg=NONE
	hi! underlined guifg=#6c71c4 gui=underline
  hi! pmenu guibg=#242424
	hi! keyword guifg=#b58900
  hi! visual guibg=#393939 guifg=default
  hi! error guibg=NONE guifg=#B82E1D
	hi! warning guibg=NONE guifg=#bfbf12
	" hi! string guifg=#859900
	hi! diffadd guibg=NONE guibg=#3D4700
	hi! diffdelete guifg=NONE guibg=#610E17 
	hi! diffchange guibg=NONE guibg=#BFBF12
  hi! comment gui=italic guifg=#4a4a4a 
	hi! statuslinenc guifg=#575757 guibg=#212121 gui=NONE
	hi! link termcursornc normal
	hi! link tabline statuslinenc
	hi! link tablinefill pmenu
	hi! link tablinesel normal
	hi! link specialkey special
  hi! link folded pmenu
	hi! link foldcolumn signcolumn 
	hi! link cursorcolumn signcolumn 
	hi! link colorcolumn signcolumn 
  hi! link statement keyword 
  hi! link delimiter default
  hi! link preproc default
	hi! link type default
	hi! link cocerrorsign warningmsg 
	hi! link cocwarningsign conceal 
	hi! link cochighlighttext visual 
	hi! link cocunderline spellbad
	hi! link cocerrorfloat diffdelete 
	hi! link cocwarningfloat diffchange 
	hi! link clojuremacro keyword
	hi! link clojuredefine keyword
	hi! link clojurekeyword identifier 
	hi! link clojurespecial keyword
	hi! link specialcomment comment
	hi! link special constant 
	hi! link operator default 
  hi! link foldcolumn signcolumn 
	hi! link fzf1 comment
	hi! link fzf2 comment
	hi! link fzf3 comment
	hi! cocerrorfloat guibg=NONE guifg=#B82E1D
	hi! cocwarningfloat guibg=NONE guifg=#bfbf12
	hi! link coccodelens linenr
	hi! link matchparen incsearch
endfunction
command! MySolarized2 call Solarized2()

"" colors
call Solarized2()

"" gutentags
let g:gutentags_ctags_exclude = ["*.min.js", "*.min.css", "build", "vendor", ".git", "node_modules", "*.vim/bundle/*" , "*/target/*"]

"" fzf/skim
" let g:fzf_tags_command = 'ctags --languages=Rust,Go,Clojure,Java,JavaScript,JSON -R' " gutentags generates
au FileType fzg tnoremap <C-e> <Esc>
let g:fzf_buffers_jump = 1
let g:fzf_layout = { 'down': '~25%' }
let g:fzf_colors = {
  \ 'fg':      ['fg', 'Normal'], 
	\ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'keyword'], 
	\ 'hl+':     ['fg', 'warning'],
	\ 'fg+':     ['fg', 'warning', 'normal', 'normal'],
  \ 'bg+':     ['bg', 'normal', 'normal' ],
  \ 'info':    ['fg', 'Comment'], 'border':  ['fg', 'Comment'],
  \ 'prompt':  ['fg', 'Function'], 'pointer': ['fg', 'Keyword'],
  \ 'marker':  ['fg', 'Type'], 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

"" autopairs
let g:AutoPairsShortcutToggle = ''
let g:AutoPairsShortcutJump = ''
let g:AutoPairsShortcutBackInsert = ''
let g:AutoPairsShortcutFastWrap = ''

"" gitgutter
let g:gitgutter_sign_modified ='·'
let g:gitgutter_sign_added='﹢'
let g:gitgutter_sign_removed='·'
let g:gitgutter_sign_modified_removed='·'
let g:gitgutter_override_sign_column_highlight=0
let g:gitgutter_sign_allow_clobber=1

"" vimrooter
let g:rooter_silent_chdir = 1

"" lightline
let g:lightline = {
  \ 'mode_map': { 'n' : 'N', 'i' : 'I', 'R' : 'R', 'v' : 'V', 'V' : 'VL' },
  \ 'colorscheme': 'powerline', 'component_function': { 'gitbranch': 'fugitive#head' },
  \ 'inactive': {
  \   'right': [ ],
  \   'left': [ [ 'filename', 'modified' ] ] },
  \ 'active': {
  \   'right': [ [ 'lineinfo' ], ['fileencoding', 'filetype'] ],
  \   'left': [ [ 'mode', 'paste' ], [ 'readonly', 'filename', 'modified','gitbranch' ] ] }, }
"" statusline

set statusline=%*
set statusline+=\ %F\ \ 
set statusline+=%{exists('g:loaded_fugitive')?fugitive#statusline():''}
set statusline+=%m
set statusline+=%=
set statusline+=%l,%.4c
set statusline+=\ %y\ %{&fileencoding?&fileencoding:&encoding}\ 

"" nerdtree
let nerdtree_tabs_focus_on_files=1
let NERDTreeHijackNetrw=0
let NERDTreeQuitOnOpen=1
let NERDTreeHighlightCursorline=1
let NERDTreeMouseMode=3
let NERDTreeMapActivateNode=">"
let NERDTreeMapPreview="<"
let NERDTreeMapChangeRoot="R"
let NERDTreeMapRefreshRoot="C"
let NERDTreeWinSize=25
let NERDTreeDirArrowExpandable = ''
au FileType * if &ft == "nerdtree" | nnoremap go :wincmd l<cr> | nnoremap go <C-w>w | endif
au FileType nerdtree setlocal signcolumn=no
au FileType nerdtree setlocal shiftwidth=1
au BufRead,BufNewFile * setlocal signcolumn=yes

"" fugitive
au FileType fugitive nnoremap <silent> <C-n> :NERDTreeToggle<cr>

"" rust/rust.vim
let g:rustfmt_autosave = 0 " coc autosaves
let g:rustfmt_emit_files = 0
let g:rustfmt_fail_silently = 0
au BufReadPost *.rs setlocal filetype=rust
au FileType rust nnoremap <silent> <buffer> <space>r :w<cr>:call CargoCmd("run", expand('%:t:r')) <cr>
au FileType rust nnoremap <silent> <buffer> <space>t :w<cr>:call CargoCmd("test", expand('%:t:r')) <cr>
au FileType rust nnoremap <silent> <buffer> <space>c :w<cr>:call CargoCmd("check", expand('%:t:r')) <cr>
au FileType rust set foldmethod=manual
au FileType rust nnoremap za zfa}
au FileType rust nnoremap <buffer> <M-z> :!zeal rust: x &<cr><cr>
au FileType rust nnoremap <silent> K :call CocAction('doHover')<cr>
au FileType rust nnoremap <silent> <f1> :call CocAction('doHover')<cr>
function! CargoCmd(cmd,bin)
	let dir = getcwd() 
	botright Topen
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
au CursorHold * silent call CocActionAsync('highlight')
nnoremap <silent> <space>q :exe 'CocList -I --input='.expand('<cword>').' grep'<CR>
au User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')

"" json
au FileType json set foldmethod=manual
au FileType json nnoremap za zfa}

"" sexp 
let g:sexp_mappings = { 
  \ 'sexp_capture_next_element': '<M-l>',  'sexp_emit_tail_element': '<M-h>',
  \ 'sexp_swap_list_backward': '',  'sexp_swap_list_forward': '', }

"" clojure
au FileType clojure nmap <space>e cpp
au FileType clojure nmap gd [<C-D>
au FileType clojure nnoremap <C-c><C-k> :%Eval<cr>
au FileType clojure nnoremap <C-c>k :%Eval<cr>
au FileType clojure nnoremap <C-c>p :execute "normal! cpp\<cr>:Last\<cr>"
au FileType clojure nnoremap <C-c><C-p> :execute "normal! cpp\<cr>:Last\<cr>"
au FileType clojure nnoremap <silent> K :call CocAction('doHover')<cr>
au FileType clojure nnoremap <silent> <f1> :call CocAction('doHover')<cr>
au FileType clojure nnoremap <buffer> <M-z> :!zeal clojure: x &<cr><cr>

"" remember cursor position
augroup vimrc-remember-cursor-position
  autocmd!
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
augroup END

"" rg/grep - usage: :Grep <search> <file> 
if executable('rg')
 set grepprg=rg\ --no-heading\ --vimgrep
 set grepformat=%f:%l:%c:%m
endif
augroup quickfix
    autocmd!
    au QuickFixCmdPost cgetexpr cwindow
    au QuickFixCmdPost lgetexpr lwindow
augroup END
function! Grep(args)
    let args = split(a:args, ' ')
    return system(join([&grepprg, shellescape(args[0]), len(args) > 1 ? join(args[1:-1], ' ') : ''], ' '))
endfunction
command! -nargs=+ -complete=file_in_path -bar MyGrep cgetexpr Grep(<q-args>)

"" neoterm
let g:neoterm_size=15
let g:neoterm_autoinsert=1
au BufWinEnter,WinEnter term://* startinsert
au BufLeave term://* stopinsert
au TermOpen * setlocal nonumber
au TermOpen * startinsert
au FileType neoterm  nnoremap <silent> <buffer> <C-j> <C-\><C-N><C-w><C-p>:Tclose<cr>
nnoremap <silent> <C-j> :botright Ttoggle<cr><cr>
inoremap <silent> <C-j> :botright Ttoggle<cr><cr>
tnoremap <silent> <C-j> <C-\><C-N><C-w><C-p>:Ttoggle<cr>
