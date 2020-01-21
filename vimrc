" plugins
call plug#begin('~/.config/nvim/plugged')
Plug 'flazz/vim-colorschemes'
Plug 'chriskempson/base16-vim'
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
Plug 'Olical/conjure', { 'do': 'bin/compile', 'for': 'clojure', 'on': 'ConjureUp' }
" Plug 'Olical/conjure', { 'tag': 'v2.1.2', 'do': 'bin/compile' }
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-salve'
Plug 'guns/vim-sexp', {'for': 'clojure'}
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf'
Plug 'machakann/vim-highlightedyank'
Plug 'christoomey/vim-tmux-navigator'
call plug#end()

"" settings
set encoding=utf-8 fileencoding=utf-8 fileencodings=utf-8 spelllang=en_us
set scrolloff=2 tabstop=2 shiftwidth=2 " set t_Co=256 completeopt-=preview 
set noswapfile nobackup noshowmode nowrap noshowcmd nospell nofoldenable
set termguicolors number cursorline hidden ttyfast ignorecase hlsearch autoread
set wildmode=list:longest,full 
set splitright splitbelow
" set fillchars=fold:\ ,stlnc:\ ,vert:│  nolist
set fillchars=fold:\ ,vert:│  nolist
set listchars=tab:→\ ,space:·,nbsp:␣,trail:•,eol:$,precedes:«,extends:»
set mouse=a clipboard=unnamed,unnamedplus guioptions=egmrti
set undodir=~/.undodir undofile
set updatetime=100 inccommand=nosplit
set sessionoptions=blank,curdir,help,tabpages,winsize
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*/.git/*,*/target/*,*~
au BufWinEnter,WinEnter * set cursorline 
au BufLeave * set nocursorline 
command! -bar -range Execute silent <line1>,<line2>yank z | let @z = substitute(@z, '\n\s*\\', '', 'g') | @z
syntax enable
filetype plugin indent on

"" key mappings
let maplocalleader = ","
nmap f <Plug>(easymotion-bd-f)
nmap <M-x> :
nnoremap Q :q!<cr>
inoremap jk <Esc>
cnoremap jk <C-c>
nnoremap <silent> <C-n> :NERDTreeToggle<CR>
nnoremap <space>o :only<cr><space><bs>
nnoremap <space>w :update<cr><space><bs>
nnoremap <space><Tab> :b#<cr><space><bs>
nnoremap <space>i :BTags<cr>
nnoremap <space>g :Rg<cr>
nnoremap <space>b :Buffers<cr>
nnoremap <space>f :Files<cr>
nnoremap <space>h :History<cr>
nnoremap <C-s> :BLines<cr>
nnoremap <C-p> :GFiles<cr>
nnoremap go <C-w>w
nnoremap gj } 
nnoremap gk {
nnoremap dh d0
nnoremap dl d$
nnoremap yl y$
nnoremap yh y0
nnoremap gl $
nnoremap gh 0
nnoremap gm %
nnoremap <C-x><C-s> :write<cr>
nnoremap <C-x><C-k> :hide<cr>
nnoremap <C-S-F> :Rg<cr>
nnoremap <C-x>k :close<cr>
nnoremap <C-q> :cq<cr>
inoremap <C-q> <esc>:cq<cr>
tnoremap <C-q> <C-\><C-N>:cq<cr>
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

function! Github()
	syntax reset
	set background=light
	hi! cursor ctermbg=blue guibg=blue
	colorscheme github
	hi! incsearch guifg=#cb4b16 gui=standout
	hi! normal guifg=#24292E guibg=#F5F5F5
  hi! pmenu guibg=#EFEFF5 guifg=#24292E
	hi! GitGutterAdd guibg=#F5F5F5 guifg=green
	hi! GitGutterDelete guibg=#F5F5F5 guifg=red
	hi! GitGutterChange guibg=#F5F5F5 guifg=yellow
  hi! VertSplit guibg=NONE
  hi! cursorLineNr guifg=black
	hi! cursorline guibg=#EAEAF0
	hi! linenr guibg=NONE guifg=#BEC0C1
  hi! statement guifg=#D73A49
	hi! function guifg=#6C40BD ""5C37A1 6F42C1
	hi! underlined guifg=#6c71c4 gui=underline
	hi! string guifg=green
	hi! signcolumn guibg=NONE
  hi! link delimiter default
  hi! link preproc default
	hi! link type default
	hi! link operator default 
	hi! link matchparen incsearch
	hi! link special statement
	hi! link cocwarningsign directory 
	hi! link identifier directory
	hi! link endofbuffer nontext
	hi! link cocerrorsign warningmsg 
	hi! link cocwarningsign warningmsg 
	hi! cocerrorfloat guibg=NONE guifg=#B82E1D
	hi! cocwarningfloat guibg=NONE guifg=#94682B
	hi! link cochighlighttext visual 
	hi! link cocunderline spellbad
	hi! statuslinenc guifg=#BDBDBD guibg=#EDEDED gui=NONE
	hi! statusline guibg=#CFCFCF
  " unlet g:loaded_fzf
	" call plug#load('fzf', 'fzf.vim')
endfunction
command! MyGithub call Github()

function! Solarized()
	syntax reset
	set background=dark
	colorscheme solarized8 
	hi! special guifg=#839496
	hi! special guifg=#b58900
  hi! VertSplit guibg=NONE guifg=#004252
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
	hi! link fzf1 comment
	hi! link fzf2 comment
	hi! link fzf3 comment
  hi! matchparen guifg=#dc322f
endfunction
command! MySolarized call Solarized()

function! Dark()
	syntax reset
	set background=dark
	colorscheme solarized8 
	" hi! normal guifg=#a3a3a3 guibg=#1A1A1A
	hi! normal guifg=#9E9E9E guibg=#1A1A1A
	hi! statement guifg=#946F00
	hi! constant guifg=#258C85
	hi! statusline guifg=#7A7A7A guibg=#1A1A1A 
	hi! function guifg=#217AB8
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
  hi! visual guibg=#9E9E9E guifg=default
  hi! error guibg=NONE guifg=#B82E1D
	hi! warning guibg=NONE guifg=#bfbf12
	hi! diffadd guibg=NONE guibg=#3D4700
	hi! diffdelete guifg=NONE guibg=#610E17 
	hi! diffchange guibg=NONE guibg=#BFBF12
	hi! GitGutterAdd guibg=NONE guifg=green
	hi! GitGutterDelete guibg=NONE guifg=darkred
	hi! GitGutterChange guibg=NONE guifg=orange
  hi! comment gui=italic guifg=#4a4a4a 
	hi! statuslinenc guifg=#575757 guibg=#212121 gui=NONE
	hi! link endofbuffer linenr
	hi! link termcursornc normal
	hi! link tabline statuslinenc
	hi! link tablinefill pmenu
	hi! link tablinesel normal
	hi! link specialkey special
  hi! link folded pmenu
	hi! link foldcolumn signcolumn 
	hi! link cursorcolumn signcolumn 
	hi! link colorcolumn signcolumn 
  hi! link keyword statement 
  hi! link delimiter default
  hi! link preproc default
	hi! link type default
	hi! cocerrorfloat guibg=NONE guifg=#cb4b16
	hi! cocwarningfloat guibg=NONE guifg=#bfbf12
	hi! link cocerrorsign cocerrorfloat
	hi! link cocwarningsign cocwarningfloat 
	hi! link cochighlighttext visual 
	hi! link cocunderline spellbad
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
	hi! link coccodelens linenr
	hi! link matchparen title
endfunction
command! MyDark call Dark()

"" default theme 
call Solarized()

"" conjure
autocmd BufEnter /tmp/conjure.cljc nnoremap <buffer> q :ConjureCloseLog<CR>
let g:conjure_log_blacklist = ["up", "ret", "load-file", "eval"]
let g:conjure_default_mappings = v:false
let g:conjure_log_direction = "vertical"
let g:conjure_omnifunc = v:false
let g:conjure_log_auto_close = v:false
 
"" highlightedyank
let g:highlightedyank_highlight_duration = 200

"" gutentags
let g:gutentags_ctags_exclude = ["*.min.js", "*.min.css", "build", "vendor", ".git", "node_modules", "*.vim/bundle/*" , "*/target/*"]

"" fzf/skim
" let g:fzf_tags_command = 'ctags --languages=Rust,Go,Clojure,Java,JavaScript,JSON -R' " gutentags generates
au FileType fzg tnoremap <C-e> <Esc>
let g:fzf_buffers_jump = 1
let g:fzf_layout = { 'down': '~18%' }
let g:fzf_colors = {
  \ 'fg':      ['fg', 'Normal'], 
	\ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'keyword'], 
	\ 'hl+':     ['fg', 'keyword'],
	\ 'fg+':     ['fg', 'keyword', 'normal', 'normal'],
  \ 'bg+':     ['bg', 'normal', 'normal' ],
  \ 'info':    ['fg', 'Comment'], 'border':  ['fg', 'Comment'],
  \ 'prompt':  ['fg', 'Directory'], 'pointer': ['fg', 'Keyword'],
  \ 'marker':  ['fg', 'Keyword'], 'spinner': ['fg', 'Label'],
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
let g:rooter_patterns = ['project.clj','deps.edn', '.git/', 'Cargo.toml']

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

"" tmux_navigator
let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <C-w>h :TmuxNavigateLeft<cr>
nnoremap <silent> <C-w>j :TmuxNavigateDown<cr>
nnoremap <silent> <C-w>k :TmuxNavigateUp<cr>
nnoremap <silent> <C-w>l :TmuxNavigateRight<cr>
nnoremap <silent> <C-w>p :TmuxNavigatePrevious<cr>
tmap <silent> <C-w>h jk:TmuxNavigateLeft<cr>
tmap <silent> <C-w>j jk:TmuxNavigateDown<cr>
tmap <silent> <C-w>k jk:TmuxNavigateUp<cr>
tmap <silent> <C-w>l jk:TmuxNavigateRight<cr>
tmap <silent> <C-w>p jk:TmuxNavigatePrevious<cr>
inoremap <silent> <C-w>h <esc>:TmuxNavigateLeft<cr>
inoremap <silent> <C-w>j <esc>:TmuxNavigateDown<cr>
inoremap <silent> <C-w>k <esc>:TmuxNavigateUp<cr>
inoremap <silent> <C-w>l <esc>:TmuxNavigateRight<cr>
inoremap <silent> <C-w>p <esc>:TmuxNavigatePrevious<cr>

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
au FileType rust nnoremap <M-z> :!zeal 'rust:'<cword> &<cr><cr>
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
nnoremap <silent> <space>q :exe 'CocList -I --input='.expand('<cword>').' grep'<CR>
nnoremap <silent> <C-h> :call CocActionAsync('highlight')<cr>
au User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')

"" json
au FileType json set foldmethod=manual
au FileType json nnoremap za zfa}

"" sexp 
let g:sexp_filetypes = ''
function! s:vim_sexp_mappings()
	nmap <silent><buffer> <M-h>  <Plug>(sexp_emit_tail_element)
	imap <silent><buffer> <M-h>  <esc><Plug>(sexp_emit_tail_element)i
	nmap <silent><buffer> <M-l>  <Plug>(sexp_capture_next_element)
	imap <silent><buffer> <M-l>  <esc><Plug>(sexp_capture_next_element)i
	imap <silent><buffer> <BS>     <Plug>(sexp_insert_backspace)
endfunction
augroup VIM_SEXP_MAPPING
	autocmd!
	au FileType clojure,scheme,lisp,timl call s:vim_sexp_mappings()
augroup END

"" clojure
au FileType clojure nnoremap <buffer> <C-c><C-k> :ConjureEvalBuffer<cr>
au FileType clojure nnoremap <buffer> <C-c>k :ConjureEvalBuffer<cr>
au FileType clojure nnoremap <buffer> <C-c><C-p> :ConjureToggleLog<cr>
au FileType clojure nnoremap <buffer> <C-c>p :ConjureToggleLog<cr>
au FileType clojure nnoremap <buffer> <space>e :ConjureEvalCurrentForm<cr>
au FileType clojure nnoremap <buffer> <space>r :ConjureEvalCurrentForm<cr>
au FileType clojure nnoremap <buffer> <space>t :ConjureRunTests<cr>
au FileType clojure nnoremap <buffer> <silent> K :call CocAction('doHover')<cr>
au FileType clojure nnoremap <buffer> <silent> <f1> :call CocAction('doHover')<cr>
au FileType clojure nnoremap <M-z> :!zeal 'clojure:'<cword> &<cr><cr>

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
command! -nargs=+ -complete=file_in_path -bar Grep cgetexpr Grep(<q-args>)

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
