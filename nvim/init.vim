" plugins
call plug#begin('~/.config/nvim/plugged')
Plug 'pechorin/any-jump.vim'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'sainnhe/gruvbox-material'
Plug 'rakr/vim-one'
Plug 'drewtempelmeyer/palenight.vim'
Plug 'dracula/vim', { 'name': 'dracula' }
Plug 'ayu-theme/ayu-vim'
Plug 'rakr/vim-one'
Plug 'morhetz/gruvbox'
Plug 'chriskempson/base16-vim'
Plug 'mbbill/undotree'
Plug 'lifepillar/vim-solarized8'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'kassio/neoterm'
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'cespare/vim-toml'
Plug 'easymotion/vim-easymotion'
Plug 'jiangmiao/auto-pairs', { 'tag': 'v2.0.0' }
Plug 'rust-lang/rust.vim', {'for': 'rust'}
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sensible'
Plug 'guns/vim-sexp', {'for': 'clojure'}
" Plug 'liuchengxu/vim-clap', { 'do': ':Clap install-binary' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf'
Plug 'machakann/vim-highlightedyank'
Plug 'christoomey/vim-tmux-navigator'
Plug 'pseewald/vim-anyfold' 
Plug 'majutsushi/tagbar'
Plug 'farmergreg/vim-lastplace'
Plug 'liuchengxu/vista.vim'
" Plug 'ludovicchabant/vim-gutentags'
Plug 'jasilven/redbush', { 'do': 'cargo install --path .' }
" Plug '/home/jari/dev/rust/redbush/plugin'
"
call plug#end()
"" settings
set guicursor=n-v-c:block-Cursor/lCursor-blinkon0,i-ci-ve:ver30-Cursor/lCursor,r-cr:hor20-Cursor/lCursor
set signcolumn=yes:1
set encoding=utf-8 fileencoding=utf-8 fileencodings=utf-8 spelllang=en_us
set smarttab tabstop=3 shiftwidth=4 expandtab " set t_Co=256 completeopt-=preview 
set noswapfile nobackup noshowmode nowrap noshowcmd nospell nofoldenable
set termguicolors number cursorline hidden ttyfast ignorecase hlsearch autoread
set wildmode=list:longest,full 
set splitright splitbelow
set scrolloff=2 nolist
set fillchars=eob:\ ,vert:│
set listchars=tab:→\ ,space:·,nbsp:␣,trail:•,eol:$,precedes:«,extends:»
set mouse=a clipboard=unnamed guioptions=egmrti
set guioptions+=a
set undodir=~/.cache/.undodir undofile
set updatetime=100 inccommand=nosplit shortmess=I 
set sessionoptions=blank,curdir,help,tabpages,winsize
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*/.git/*,*/target/*,*~
au BufWinEnter,WinEnter * set cursorline 
au BufLeave * set nocursorline 
command! -bar -range Execute silent <line1>,<line2>yank z | let @z = substitute(@z, '\n\s*\\', '', 'g') | @z
set synmaxcol=130
syntax enable
filetype plugin indent on

set mouse=a clipboard=unnamedplus guioptions=egmrti
"" key mappings
let maplocalleader = ","
nnoremap <silent> <2-LeftMouse> :let @/='\V\<'.escape(expand('<cword>'), '\').'\>'<cr>:set hls<cr>
nnoremap <silent> * :let @/='\V\<'.escape(expand('<cword>'), '\').'\>'<cr>:set hls<cr>
nmap f <Plug>(easymotion-bd-f)
nmap <M-x> :
nnoremap Q :q!<cr>
inoremap jk <Esc>
" cnoremap jk <C-c>
nnoremap <silent> <C-n> :NERDTreeToggle<cr>
nnoremap <silent> <C-t> :TagbarToggle<cr>
nnoremap <space>o :only<cr><space><bs>
nnoremap <space>w :w<cr><space><bs>
nnoremap <space><Tab> :b#<cr><space><bs>
nnoremap <space>i :BTags<cr>
nnoremap <space>j :AnyJump<cr>
nnoremap <space>g :Rg<cr>
nnoremap <space>b :Buffers<cr>
nnoremap <space>f :Files<cr>
nnoremap <space>h :History<cr>
nnoremap <C-s> :BLines<cr>
nnoremap <C-p> :GFiles<cr>
nnoremap <C-o> <C-o>zz
nnoremap go <C-w>w
nnoremap <M-j> } 
nnoremap <M-k> {
nnoremap dh d0
nnoremap dl d$
nnoremap yl y$
nnoremap yh y0
nnoremap gl $
nnoremap gh 0
nnoremap gm %
nnoremap <C-x><C-s> :write<cr>
nnoremap <C-S-F> :Rg<cr>
nnoremap <C-x><C-k> :bd!<cr>
nnoremap <C-x>k :bd!<cr>
tnoremap <C-x><C-k> <C-\><C-N>:bd!<cr>
tnoremap <C-x>k <C-\><C-N>:bd!<cr>
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
" nnoremap <M-n> :lnext<cr>
" nnoremap <M-p> :lprev<cr>
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
nmap <RightMouse> <LeftMouse>gdzz
nmap <RightMouse> <LeftMouse>gdzz
nmap <MiddleMouse> <C-o>
nmap <C-RightMouse> <C-o>
nmap <RightMouse> <LeftMouse>gdzz
" vnoremap  <space>y  "+y
" nnoremap  <space>y  "+y

"" anyjump
let g:any_jump_window_width_ratio  = 0.8
let g:any_jump_window_height_ratio = 0.3
nnoremap  <leader>y  "+y
"" easymotion
let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxy'

"" tagbar
let g:tagbar_left = 1
let g:tagbar_compact = 1
let g:tagbar_width = 25
let g:tagbar_indent = 1

"" highlightedyank
let g:highlightedyank_highlight_duration = 200

" "" gutentags
" let g:gutentags_ctags_exclude = ["*.min.js", "*.min.css", "build", "vendor", ".git", "node_modules", "*.vim/bundle/*" , "target", "classes"]

"" fzf
let g:fzf_tags_command = 'ctags --languages=Rust,Go,Clojure,Java,JavaScript,JSON -R'
au FileType fzg tnoremap <C-e> <Esc>
let g:fzf_preview_window = ''
let g:fzf_buffers_jump = 1
let g:fzf_layout = { 'down': '~18%' }
let g:fzf_colors = {
    \ 'fg':      ['fg', 'Comment'], 
    \ 'bg':      ['bg', 'Normal'],
    \ 'hl':      ['fg', 'Function'], 
    \ 'hl+':     ['fg', 'Function'],
    \ 'fg+':     ['fg', 'Keyword', 'Keyword', 'Keyword'],
    \ 'bg+':     ['bg', 'Search', 'Search' ],
    \ 'info':    ['fg', 'Comment'], 'border':  ['fg', 'Comment'],
    \ 'prompt':  ['fg', 'Directory'], 'pointer': ['fg', 'Keyword'],
    \ 'marker':  ['fg', 'Keyword'], 'spinner': ['fg', 'Label'],
    \ 'header':  ['fg', 'Normal'] }
if has('nvim')
    function! FloatingFZF(width, height, border_highlight)
        function! s:create_float(hl, opts)
            let buf = nvim_create_buf(v:false, v:true)
            let opts = extend({'relative': 'editor', 'style': 'minimal'}, a:opts)
            let win = nvim_open_win(buf, v:true, opts)
            call setwinvar(win, '&winhighlight', 'NormalFloat:'.a:hl)
            call setwinvar(win, '&colorcolumn', '')
            return buf
        endfunction
        " Size and position
        let width = float2nr(&columns * a:width)
        let height = float2nr(&lines * a:height)
        let row = float2nr((&lines - height) / 2)
        let col = float2nr((&columns - width) / 2)
        " Border
        let top = '╭' . repeat('─', width - 2) . '╮'
        let mid = '│' . repeat(' ', width - 2) . '│'
        let bot = '╰' . repeat('─', width - 2) . '╯'
        let border = [top] + repeat([mid], height - 2) + [bot]
        " Draw frame
        let s:frame = s:create_float(a:border_highlight, {'row': row, 'col': col, 'width': width, 'height': height})
        call nvim_buf_set_lines(s:frame, 0, -1, v:true, border)
        " Draw viewport
        call s:create_float('Normal', {'row': row + 1, 'col': col + 2, 'width': width - 4, 'height': height - 2})
        autocmd BufWipeout <buffer> execute 'bwipeout' s:frame
    endfunction
    let g:fzf_layout = { 'window': 'call FloatingFZF(0.7, 0.2, "Comment")' }
endif
command! -bang -nargs=* Rg call fzf#vim#grep('rg --column --line-number --fixed-strings --ignore-case --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)

"" autopairs
let g:AutoPairsShortcutToggle = ''
let g:AutoPairsShortcutJump = ''
let g:AutoPairsShortcutBackInsert = ''
let g:AutoPairsShortcutFastWrap = ''

"" gitgutter
let g:gitgutter_sign_modified ='~'
let g:gitgutter_sign_added='+'
let g:gitgutter_sign_removed='-'
let g:gitgutter_sign_modified_removed='-'
let g:gitgutter_override_sign_column_highlight=0
let g:gitgutter_sign_allow_clobber=0
let g:gitgutter_set_sign_backgrounds=1

"" vimrooter
let g:rooter_silent_chdir = 1
let g:rooter_patterns = ['project.clj','deps.edn', '.git/', 'Cargo.toml', 'go.mod']

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
let NERDTreeQuitOnOpen=0
let NERDTreeHighlightCursorline=1
let NERDTreeMouseMode=1
let NERDTreeMapActivateNode=">"
let NERDTreeMapPreview="<tab>"
let NERDTreeMapChangeRoot="R"
let NERDTreeMapRefreshRoot="C"
let NERDTreeWinSize=25
" let NERDTreeDirArrowExpandable = '+'
" let NERDTreeDirArrowCollapsible = ' '
let NERDTreeShowHidden=1
au FileType nerdtree nnoremap go <C-w>p

""nnoremap go :wincmd l<cr>
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

"" vista
let g:vista_executive_for = { 'rust': 'coc', }
let g:vista_icon_indent = ["╰─▸ ", "├─▸ "]
let g:vista#renderer#enable_icon = 0
let g:vista#renderer#icons = { "function": "F", "variable": "V", }

"" rust/rust.vim
let g:rustfmt_autosave = 0
let g:rustfmt_emit_files = 0
let g:rustfmt_fail_silently = 0
au BufReadPost *.rs setlocal filetype=rust
au FileType rust nnoremap <silent> <buffer> <space>r :w<cr>:call CargoCmd("cargo run", expand('%:t:r')) <cr>
au FileType rust nnoremap <silent> <buffer> <space>t :w<cr>:call CargoCmd("RUST_BACKTRACE=1 cargo test", expand('%:t:r')) <cr>
au FileType rust nnoremap <silent> <buffer> <space>c :w<cr>:call CargoCmd("cargo check", expand('%:t:r')) <cr>
au FileType rust set foldmethod=manual
" au FileType rust nnoremap za zfa}
au FileType rust nnoremap <M-z> :!zeal 'rust:'<cword> &<cr><cr>
function! CargoCmd(cmd,bin)
    let dir = getcwd() 
    botright Topen
    T date
    if a:bin == "main"
        exe "T cd ". dir . " && " . a:cmd 
    else
        exe "T cd ". dir . " && " . a:cmd . " --bin ". a:bin
    endif
endfunction

"" coc
nmap <f2> <Plug>(coc-rename)
nmap <silent> <M-n> <Plug>(coc-diagnostic-next)zz
nmap <silent> <M-p> <Plug>(coc-diagnostic-prev)zz
nmap <silent> gd <Plug>(coc-definition)zz
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> gA <Plug>(coc-codelens-action)
nmap <silent> ga :CocCommand actions.open<cr>
nnoremap <silent> <space>q :exe 'CocList -I --input='.expand('<cword>').' grep'<CR>
nnoremap <silent> <C-h> :call CocActionAsync('highlight')<cr>
au FileType rust,go,clojure nnoremap <silent> K :call CocAction('doHover')<cr>
au User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')

"" json
" au FileType json setlocal foldmethod=manual
au FileType json nnoremap za zfa}

"" sexp 
let g:sexp_filetypes = ''
function! s:vim_sexp_mappings()
    nmap <silent><buffer> <M-h>  <Plug>(sexp_emit_tail_element)
    imap <silent><buffer> <M-h>  <esc><Plug>(sexp_emit_tail_element)i
    nmap <silent><buffer> <M-l>  <Plug>(sexp_capture_next_element)
    imap <silent><buffer> <M-l>  <esc><Plug>(sexp_capture_next_element)i
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
au FileType clojure nnoremap <buffer> <space>x :ConjureEvalRootForm<cr>
au FileType clojure nnoremap <buffer> <space>r :ConjureEvalCurrentForm<cr>
au FileType clojure nnoremap <buffer> <space>t :ConjureRunTests<cr>
au FileType clojure nnoremap <M-z> :!zeal 'clojure:'<cword> &<cr><cr>

"" conjure
au BufEnter */conjure.cljc nnoremap <buffer> q :ConjureCloseLog<CR>
au BufEnter */conjure.cljc setlocal nonumber 
let g:conjure_log_blacklist = ["up", "ret", "load-file", "eval"]
let g:conjure_default_mappings = v:false
let g:conjure_log_direction = "vertical"
let g:conjure_omnifunc = v:false
let g:conjure_log_auto_close = v:false
let g:conjure_fold_multiline_results = v:true
let g:conjure_quick_doc_time = 600
let g:conjure_quick_doc_insert_mode = v:false
let g:conjure_quick_doc_normal_mode = v:false
" au FileType clojure nnoremap za zfa}

"" anyfold
au Filetype * AnyFoldActivate
au BufReadPre,BufRead,BufWinEnter,BufNewFile,BufEnter conjure.cljc setlocal foldmethod=marker

"" neoterm
let g:neoterm_size=17
let g:neoterm_autoinsert=1
au BufWinEnter,WinEnter term://* startinsert
au BufLeave term://* stopinsert
au TermOpen * setlocal nonumber
au TermOpen * startinsert
au FileType neoterm  nnoremap <silent> <buffer> <C-j> <C-\><C-N><C-w><C-p>:Tclose<cr>
nnoremap <silent> <C-j> :botright Ttoggle<cr><cr>
inoremap <silent> <C-j> :botright Ttoggle<cr><cr>
tnoremap <silent> <C-j> <C-\><C-N><C-w><C-p>:Ttoggle<cr>

"" redbush
" let g:redbush_bin = '/home/jari/dev/rust/redbush/target/debug/redbush'
let g:redbush_bin = 'redbush'
let g:redbush_filepath = '/tmp/redbush-eval.clj'
let g:redbush_filesize = 1000 
let g:redbush_is_vertical = v:true
let g:redbush_winsize = 40

"" go
autocmd BufWritePre *.go :call CocAction('runCommand', 'editor.action.organizeImport')

"" gruvbox
function! MyGruvBox(background)
    syntax reset
    if a:background == "dark"
        set background=dark
        let g:gruvbox_contrast_dark='hard'
        colorscheme gruvbox 
        hi! gruvboxred guifg=#fe8019
        hi! visual gui=NONE
    else
        set background=light
        let g:gruvbox_contrast_dark='hard'
        colorscheme gruvbox 
    end
    hi! linenr guibg=normal
    hi! signcolumn guibg=normal
    hi! link SignColumn LineNr
    hi! GitGutterAdd    guibg=NONE
    hi! GitGutterChange guibg=NONE
    hi! GitGutterDelete guibg=NONE
    hi! GruvBoxRedSign guibg=NONE
    hi! GruvBoxOrangeSign guibg=NONE
    hi! GruvBoxYellowSign guibg=NONE
    hi! GruvBoxBlueSign guibg=NONE
    hi! rustmodpathsep guifg=normal
endfun
command! -nargs=1 MyGruvBox call MyGruvBox(<q-args>)

function! MySolarized(background)
    syntax reset
    colorscheme solarized8 
    if a:background == "dark"
        set background=dark
    else
        set background=light
    end
    hi! special guifg=#b58900
    " hi! highlight guibg=#0A4757
    " hi! diffdelete guifg=#9B5B59
    " hi! warningmsg guifg=#CC5E5B
    " hi! cocerrorvirtualtext guifg=#CC5E5B gui=italic
    " hi! endofbuffer guifg=#fdf6e3
    hi! link wildmenu pmenu
    hi! link cursorLineNr keyword 
    hi! link VertSplit nontext
    hi! link linenr comment
    hi! link title warningmsg
    hi! link delimiter default
    hi! link preproc default
    hi! link type default
    hi! link cocerrorsign warningmsg 
    hi! link cocwarningsign special
    hi! link cochighlighttext visual 
    " hi! link cocunderline highlight 
    hi! link cocerrorfloat diffdelete 
    hi! link cocwarningfloat diffchange 
    hi! link clojuremacro keyword
    hi! link clojuredefine keyword
    hi! link clojurekeyword identifier 
    hi! link clojurespecial keyword
    hi! link tabline pmenu
    hi! link tablinefill pmenu
    hi! link tablinesel normal
    hi! link fzf1 Comment
    hi! link fzf2 Comment
    hi! link fzf3 Comment
    hi! link rustattribute comment 
    hi! link rustderive comment 
    hi! link rustderivetrait comment 
    hi! link tagbarsignature comment 
    hi! link rustenumvariant default 
    hi! link gospaceerror default
    " hi! matchparen guifg=orange guibg=#002B36
    hi! easymotiontarget guifg=red
    hi! link NERDTreeFile default
endfunction
command! -nargs=1 MySolarized call MySolarized(<q-args>)

"" default theme 
call MyGruvBox("dark")
