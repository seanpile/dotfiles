" Map leader to space
nnoremap <Space> <nop>
let mapleader=" "

set nocompatible
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" tpope goodness
Plugin 'tpope/vim-sensible'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-commentary'

" async running
Plugin 'skywind3000/asyncrun.vim'

" FileBeagle
Plugin 'jeetsukumaran/vim-filebeagle'

" Better text/markdown editing support
Plugin 'reedes/vim-pencil'

" Dim inactive windows
Plugin 'blueyed/vim-diminactive'

" Autocomplete
Plugin 'lifepillar/vim-mucomplete'

" fzf - fuzzy finding
Plugin 'junegunn/fzf.vim'

" Modify word motions to be CamelCase/snake_case aware
Plugin 'chaoren/vim-wordmotion'

" User Interface Mods
Plugin 'altercation/vim-colors-solarized'
Plugin 'itchyny/lightline.vim'

" Fix Buffer kill behaviour
Plugin 'qpkorr/vim-bufkill'

" Better grepping
Plugin 'mileszs/ack.vim'

" Language Pack
Plugin 'sheerun/vim-polyglot'

" C/C++ Code formatter
Plugin 'rhysd/vim-clang-format'

" JavaScript
Plugin 'mitermayer/vim-prettier'
Plugin 'ruanyl/vim-sort-imports'

" Go
Plugin 'fatih/vim-go'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
syntax on

set hidden
set belloff=all
set shortmess+=c
set number
set ignorecase
set smartcase
set splitbelow
set splitright
set equalalways
set nobackup
set noswapfile
set autowrite
set wildmode=longest,list,full
set wildmenu
set noshowmode
set guioptions=
set undofile
set autoread
set title
set cursorline
set undodir=$HOME/.vim/undo
set fillchars+=vert:│
autocmd! ColorScheme * hi VertSplit ctermbg=NONE guibg=NONE

" -------------------------------
"  Speed related optimizations
" -------------------------------
if !has('gui_running')
  let loaded_matchparen=1 " Don't load matchit.vim (paren/bracket matching)
  set noshowmatch         " Don't match parentheses/brackets
  set nocursorcolumn      " Don't paint cursor column
  set lazyredraw          " Wait to redraw
  set scrolljump=8        " Scroll 8 lines at a time at bottom/top
endif


" -------------------------------
" Window Management
" -------------------------------
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Easy Navigation of Quickfix list
command! Cnext try | cnext | catch | cfirst | catch | endtry
command! Cprev try | cprev | catch | clast | catch | endtry
noremap <Left> :Cprev<CR>
noremap <Right> :Cnext<CR>
noremap <Up> :cfirst<CR>
noremap <Down> :clast<CR>
au FileType qf wincmd J

nnoremap <Leader>s :w<CR>

" Close a buffer:    <Leader> Backspace
" Close a window:    <Leader> w
" List open buffers: <Leader> b
noremap <Leader>w :close<CR>
noremap <Leader><BS> :BD<CR>

" Convenient saving mapping (in normal or insert mode)
if has('gui_win32')
    " Maps <Command>-s on Mac keyboard to save.  Note:  I added this by
    " pressing 'Ctrl-v' -> 'Command-s' while in insert mode.
    nnoremap ó :w<CR>
    inoremap ó <Esc>:w<CR>
endif


" -------------------------------
" Lightline
" -------------------------------
let g:lightline = {
	\ 'colorscheme': 'solarized',
	\ 'active': {
	\   'left': [['mode', 'paste'], ['filename', 'modified']],
	\   'right': [['lineinfo'], ['percent'], ['readonly']]
	\ },
\ }

if has('gui_running')
    " gvim specific settings here

    " Prevent q and ZZ from closing the window; in gVim mode, I prefer a more
    " long running application.  I found I kept accidentally closing the
    " window
    cabbrev q <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'close' : 'q')<CR>
    nnoremap ZZ <nop>

    " gVim allows us to use Command-S for saving a file; this autocmd
    " will pop us back into Normal mode whenever we save (which
    " seems to be more intuitive)
    autocmd BufWritePre * :stopinsert

    " -------------------------------
    " vim-colors-solarized
    " -------------------------------
    set background=dark
    colorscheme solarized

else

    " -------------------------------
    " vim-colors-solarized
    " -------------------------------
    set background=dark
    colorscheme solarized

endif

" Set font (system dependant)
set encoding=utf8
if has("gui_win32")
    set guifont=Consolas:h10:cANSI
else
    set guifont=Knack\ Regular\ Nerd\ Font\ Complete:h12
endif

" -------------------------------
" Ack.vim
" -------------------------------
nnoremap <Leader>g :Ack<space>
let g:ack_use_cword_for_empty_search = 1
if executable('rg')
    let g:ackprg = 'rg --vimgrep --no-heading'
endif

" -------------------------------
" clang-format
" -------------------------------
if executable('clang-format')
    let g:clang_format#detect_style_file=1
    let g:clang_format#auto_formatexpr=1
endif

" -------------------------------
" c/c++ options
" -------------------------------
augroup cmode
  autocmd!
  autocmd FileType c,cpp,objc ClangFormatAutoEnable
  autocmd FileType c,cpp,objc setlocal commentstring=//\ %s
  autocmd FileType c,cpp,objc setlocal formatoptions=jrql
  autocmd FileType c,cpp,objc setlocal expandtab tabstop=4 shiftwidth=4
augroup END

" -------------------------------
" Javascript Settings
" -------------------------------
augroup jsmode
  autocmd!
  let g:prettier#autoformat = 0
  autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.vue Prettier
  autocmd FileType javascript,css,jsx,scss,json setlocal expandtab tabstop=2 shiftwidth=2
augroup END

" -------------------------------
" Markdown Syntax
" -------------------------------
augroup mdmode
  autocmd!
  autocmd BufWritePre *.md Prettier
  autocmd FileType markdown setlocal expandtab tabstop=4 shiftwidth=4 autoindent colorcolumn=0 linebreak nonumber wrap textwidth=120
  autocmd FileType yaml setlocal expandtab tabstop=2 shiftwidth=2
augroup END

" -------------------------------
" Quickfix Configuration
" -------------------------------
augroup quickfix
    autocmd!
    autocmd FileType qf set nobuflisted
augroup END

" -------------------------------
" Golang Config
" -------------------------------
let g:go_list_type = "quickfix"
let g:go_fmt_autosave = 1
let g:go_fmt_command = "goimports"
let g:go_fmt_fail_silently = 0
let g:go_def_reuse_buffer = 1
let g:go_auto_type_info = 1
let g:go_echo_command_info = 1
let g:go_list_height = 10
let g:go_metalinter_enabled = ['vet', 'golint', 'errcheck', 'maligned', 'unconvert']
let g:go_highlight_functions = 0
let g:go_highlight_structs = 0
let g:go_highlight_interfaces = 0

augroup gomode
  autocmd!

  autocmd Filetype go command! -bang A call go#alternate#Switch(<bang>0, 'edit')
  autocmd Filetype go command! -bang AV call go#alternate#Switch(<bang>0, 'vsplit')
  autocmd Filetype go command! -bang AS call go#alternate#Switch(<bang>0, 'split')
  
  autocmd FileType go nmap <Leader>r <Plug>(go-run)
  autocmd FileType go nmap <Leader>i <Plug>(go-info)
  autocmd FileType go nmap <Leader>d <Plug>(go-doc)
  autocmd FileType go nmap <Leader>t :GoDecls<CR>
  autocmd FileType go nmap <Leader>T :GoDeclsDir<CR>
  autocmd FileType go setlocal noexpandtab tabstop=4 shiftwidth=4
  autocmd FileType go setlocal commentstring=//\ %s
  autocmd FileType go setlocal formatoptions=jrql
augroup END

" -------------------------------
" fzf
" -------------------------------
set rtp+=/usr/local/opt/fzf
nmap <Leader>b :Buffers<CR>
nmap <Leader>f :Files<CR>
let $FZF_DEFAULT_COMMAND="rg --files --hidden -g \!.git -g \!vendor/"

" -------------------------------
" FileBeagle
" -------------------------------
let g:filebeagle_suppress_keymaps = 1
nmap <silent> <Leader>/ <Plug>FileBeagleOpenCurrentWorkingDir
nmap <silent> -         <Plug>FileBeagleOpenCurrentBufferDir

" -------------------------------
" bufkill
" -------------------------------
let g:BufKillCreateMappings = 0

" -------------------------------
" MUComplete
" -------------------------------
set completeopt+=menuone
set completeopt+=noselect
let g:mucomplete#enable_auto_at_startup = 1
let g:mucomplete#chains = { 'sql' : ['file'] }

" -------------------------------
" vim-polyglot
" -------------------------------
let g:polyglot_disabled = ['go']

" -------------------------------
" vim-pencil
" -------------------------------
let g:pencil#wrapModeDefault='soft'
let g:pencil#autoformat=1
augroup pencil
  autocmd!
  autocmd FileType markdown,mkd call pencil#init({'wrap': 'soft'})
  autocmd FileType text         call pencil#init({'wrap': 'hard'})
augroup END

" -------------------------------
" diminactive
" -------------------------------
let g:diminactive_enable_focus = 1

" -------------------------------
" async / build management
" -------------------------------
let g:asyncrun_open = 8
let g:asyncrun_trim = 1
nnoremap <Leader>m :AsyncRun -post=execute\ "Cnext" -cwd=<root> ./build.macosx<CR>
