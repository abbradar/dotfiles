set nocompatible " be iMproved

let mapleader = ","
let maplocalleader = "\\"

filetype off " for vundle

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'

"
" Bundles and settings
"

" Zoom window
Bundle 'ZoomWin'

" TODO: wait for fixed bug #83
"" Fast buffers change
"Bundle 'LustyJuggler'

" Buffer explorer
Bundle "bufexplorer.zip"

" Undo tree
Bundle 'mbbill/undotree'

" Motion by first letters
Bundle 'Lokaltog/vim-easymotion'

" Syntax checking
Bundle 'scrooloose/syntastic'

" Git handling
Bundle 'tpope/vim-fugitive'

" Fancy status line
Bundle 'Lokaltog/vim-powerline'

" Fast file search
Bundle 'kien/ctrlp.vim'

" NERD tree
Bundle 'scrooloose/nerdtree'

" Fuzzy file finder
Bundle 'kien/ctrlp.vim'

" Color picker with <leader>ce
Bundle 'Rykka/colorv.vim'

" Universal autocompletion plugin
Bundle 'Shougo/neocomplcache'

" Snippets for neocomplcache
Bundle 'Shougo/neosnippet'
Bundle 'honza/snipmate-snippets'

" Solarized theme
Bundle 'altercation/vim-colors-solarized'

" % for matching regexps and so on
Bundle 'matchit.zip'

" Color picker
Bundle 'spf13/vim-colors'

" Automatic brackets
Bundle 'tpope/vim-surround'

" Auto abs/relative number lines
Bundle 'jeffkreeftmeijer/vim-numbertoggle'

" Autocompletion for brackets, quotes etc
Bundle 'Raimondi/delimitMate'

" Session save plugin
Bundle 'xolox/vim-session'

" Display indent levels
Bundle 'nathanaelkane/vim-indent-guides'

" Tabularizing
Bundle 'godlygeek/tabular'

" Autocommenting on <leader>cc
Bundle 'scrooloose/nerdcommenter'

" Open region in new buffer with <leader>nr
Bundle 'chrisbra/NrrwRgn'

" Handle URLs in text
Bundle 'utl.vim'

" Calendar (TODO no binding for now)
Bundle 'calendar.vim--Matsumoto'

" VimShell
Bundle 'Shougo/vimproc'
Bundle 'Shougo/vimshell'

" Various signs on lines
Bundle 'tomtom/quickfixsigns_vim'

" Smart <Space> (repeat motions)
Bundle 'spiiph/vim-space'

" Fancy Prolog
Bundle 'adimit/prolog.vim'

" Libraries
Bundle 'tomtom/tlib_vim'

" C++
"Bundle 'Shougo/neocomplcache-clang'
Bundle 'Rip-Rip/clang_complete'
Bundle 'osyo-manga/neocomplcache-clang_complete'

" Python
"Bundle 'klen/python-mode'
"Bundle 'python.vim'
"Bundle 'python_match.vim'
"Bundle 'pythoncomplete'

" SQL
"Bundle 'dbext.vim'

"
" End bundles
"

filetype plugin indent on

" Stylish!
let g:solarized_termcolors=256
colorscheme solarized
set background=dark

if has('gui_running')
  set guioptions-=T " No toolbar
  set guifont=Droid\ Sans\ Mono\ 11
  let g:vimconf=".gvimrc"
else
  let g:vimconf=".vimrc"
endif
execute "autocmd! bufwritepost " . g:vimconf . " execute 'normal! :source %'"

" Clipboard
"if has ("unix") && "Darwin" != system("echo -n \"$(uname)\"")
"  " on Linux use + register for copy-paste
"  set clipboard=unnamedplus
"else
"  " one mac and windows, use * register for copy-paste
"  set clipboard=unnamed
"endif

" Backups
set nobackup
set backupdir=~/.vim/backup
set directory=~/.vim/tmp

" Buffers
set hidden

" UI
set ruler " Ruler on
set nu " Line numbers on
set nowrap " Line wrapping off
set laststatus=2 " Always show the statusline
set cmdheight=2 " Make the command area two lines high
set wildmenu " Show possible completions for commands

" Keymap and encodings
set encoding=utf-8
"set keymap=russian-jcukenwin
set langmap=ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz
set fileencodings=utf8,cp1251,cp866

" Behaviours
syntax enable
set history=768 " Number of things to remember in history

" Text Format
set tabstop=2
set backspace=eol,start,indent " Fix backspace (allow backspace over...)
set shiftwidth=2 " Tabs under smart indent
set smartindent
set autoindent
set smarttab
set expandtab " Tabs as spaces
set wrap " Wrap lines

" Searching
set smartcase " Smart case search
set incsearch " Incremental search
set hlsearch " Highlight search results
set magic " Regular expressions

" Visual
set showmatch " Show matching brackets.
set matchtime=2 " How many tenths of a second to blink
if exists('+colorcolumn')
  set colorcolumn=80 " Color the 80th column differently
endif

" Sounds
set errorbells
set novisualbell
" OmniComplete
if has("autocmd") && exists("+omnifunc")
  autocmd Filetype *
        \if &omnifunc == "" |
        \setlocal omnifunc=syntaxcomplete#Complete |
        \endif
endif

" Files
"set autochdir

" Ctags
set tags=./tags;,~/.vim/tags

" LustyJuggler
" Show numbers for Lusty Buffers
let g:LustyJugglerShowKeys=1

" indent_guides
let g:indent_guides_auto_colors = 1
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup = 1

" Neocomplcache
" Launches neocomplcache automatically on vim startup.
let g:neocomplcache_enable_at_startup = 1
" Use smartcase.
let g:neocomplcache_enable_smart_case = 1
" Use camel case completion.
let g:neocomplcache_enable_camel_case_completion = 1
" Use underscore completion.
let g:neocomplcache_enable_underbar_completin = 1
" Sets minimum char length of syntax keyword.
let g:neocomplcache_min_syntax_length = 3

" Neosnippet
" Enables snipmate-snippets compatibility
let g:neosnippet#enable_snipmate_compatibility = 1
" Tell Neosnippet about the other snippets
let g:neosnippet#snippets_directory = '~/.vim/bundle/snipmate-snippets/snippets'

" clang_complete
let g:clang_snippets_engine = 'snipmate'
let g:clang_use_library = 1
let g:clang_library_path = '/usr/lib/llvm'
let g:clang_complete_patterns = 1

" use neocomplcache & clang_complete
let g:neocomplcache_force_overwrite_completefunc = 1
let g:clang_complete_auto = 1

" delimitMate
let delimitMate_expand_space = 1
let delimitMate_expand_cr = 1

" VimOrganizer
au! BufRead,BufWrite,BufWritePost,BufNewFile *.org
au BufEnter *.org call org#SetOrgFileType()

"
" Key mappings
"

" F1=Esc (less errors)
nmap <F1> <Esc>
imap <F1> <Esc>
vmap <F1> <Esc>

" Use ; for : (less keystrokes)
nmap ; :
vmap ; :

" Edit .vimrc
nmap <leader>vv :execute(":e ~/" . g:vimconf)<CR>

" Treat long lines as break lines (useful when moving around in them)
nnoremap <Down> gj
nnoremap <Up> gk

:cabbr <expr> %% expand('%:p:h')

" Smart way to move between windows
map <C-J> <C-W>j
map <C-K> <C-W>k
map <C-L> <C-W>l
map <C-H> <C-W>h

map <C-j> <PageDown>
map <C-k> <PageUp>

" Buffers
map <leader>bd :bd<CR> " Close the current buffer

" Code folding
nmap <leader>f0 :set foldlevel=0<CR>
nmap <leader>f1 :set foldlevel=1<CR>
nmap <leader>f2 :set foldlevel=2<CR>
nmap <leader>f3 :set foldlevel=3<CR>
nmap <leader>f4 :set foldlevel=4<CR>
nmap <leader>f5 :set foldlevel=5<CR>
nmap <leader>f6 :set foldlevel=6<CR>
nmap <leader>f7 :set foldlevel=7<CR>
nmap <leader>f8 :set foldlevel=8<CR>
nmap <leader>f9 :set foldlevel=9<CR>

" For when you forget to sudo.. Really Write the file.
cmap w!! w !sudo tee % >/dev/null

" Insert blank lines
nnoremap [<Space> maO<esc>`a
nnoremap ]<Space> mao<esc>`a

" VimShell
nmap <Leader>vs :VimShell<CR>
" Vundle
nmap <Leader>bi :BundleInstall<CR>
nmap <Leader>bu :BundleInstall!<CR> " Because this also updates
nmap <Leader>bc :BundleClean<CR>
" ZoomWin
nmap <silent> <leader>wo :ZoomWin<CR> " Zoom Window to Full Size
" NERD tree
nmap <leader>nn :NERDTreeToggle<CR>
nmap <leader>nf :NERDTreeFind<CR>
" Tabular
nmap <Leader>t= :Tabularize /=<CR>
vmap <Leader>t= :Tabularize /=<CR>
nmap <Leader>t: :Tabularize /:\zs<CR>
vmap <Leader>t: :Tabularize /:\zs<CR>
nmap <Leader>t, :Tabularize /,\zs<CR>
vmap <Leader>t, :Tabularize /,\zs<CR>
nmap <Leader>t> :Tabularize /=>\zs<CR>
vmap <Leader>t> :Tabularize /=>\zs<CR>
nmap <Leader>t- :Tabularize /-<CR>
vmap <Leader>t- :Tabularize /-<CR>
nmap <Leader>t" :Tabularize /"<CR>
vmap <Leader>t" :Tabularize /"<CR>

" UndoTree
nmap <Leader>u :UndotreeToogle<CR>

" Neosnippet
" SuperTab like snippets behavior.
imap <expr><TAB> neosnippet#expandable() ? "\<Plug>(neosnippet_expand_or_jump)" : pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

" Plugin key-mappings.
" Ctrl-k expands snippet & moves to next position
" <CR> chooses highlighted value
imap <C-k>     <Plug>(neocomplcache_snippets_expand)
smap <C-k>     <Plug>(neocomplcache_snippets_expand)
inoremap <expr><C-g>   neocomplcache#undo_completion()
inoremap <expr><C-l>   neocomplcache#complete_common_string()
inoremap <expr><CR>    neocomplcache#complete_common_string()

" <CR>: close popup
" <s-CR>: close popup and save indent.
inoremap <expr><s-CR> pumvisible() ? neocomplcache#close_popup()"\<CR>" : "\<CR>"
inoremap <expr><CR>  pumvisible() ? neocomplcache#close_popup() : "\<CR>"

" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><s-TAB>  pumvisible() ? "\<C-p>" : "\<TAB>"

" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplcache#close_popup()
