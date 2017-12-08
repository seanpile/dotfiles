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

" FileBeagle
Plugin 'jeetsukumaran/vim-filebeagle'

" Autocomplete
Plugin 'Valloric/YouCompleteMe'

" fzf - fuzzy finding
Plugin 'junegunn/fzf'
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

" Go
Plugin 'fatih/vim-go'

" Rust
Plugin 'rust-lang/rust.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
syntax on

set hidden
set belloff=all
set number
set scrolloff=999
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
set cursorline
set guioptions=
set undofile
set colorcolumn=100
set undodir=$HOME/.vim/undo

" -------------------------------
" Window Management
" -------------------------------
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Easy Navigation of Quickfix list
noremap <Left> :cprev<CR>
noremap <Right> :cnext<CR>
noremap <Up> :cfirst<CR>
noremap <Down> :clast<CR>

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

" Commenting: Make // be the default c format
autocmd FileType c,cpp setlocal commentstring=//\ %s
autocmd FileType c,cpp setlocal formatoptions=jrql

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
    set background=light
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
" Make
" -------------------------------
nnoremap <Leader>m :silent make<CR>

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
" map to <Leader>cf in C++ code
if executable('clang-format')
    autocmd FileType c,cpp,objc nnoremap <buffer><Leader>f :<C-u>ClangFormat<CR>
    autocmd FileType c,cpp,objc vnoremap <buffer><Leader>f :ClangFormat<CR>
    let g:clang_format#detect_style_file=1
    let g:clang_format#auto_formatexpr=1
endif
autocmd FileType c,cpp,objc setlocal expandtab tabstop=4 shiftwidth=4

" -------------------------------
" Javascript Settings
" -------------------------------
autocmd FileType javascript nmap <Leader>f <Plug>(Prettier)
autocmd FileType javascript setlocal expandtab tabstop=2 shiftwidth=2

" -------------------------------
" Markdown Syntax
" -------------------------------
autocmd FileType markdown setlocal expandtab tabstop=4 shiftwidth=4
autocmd FileType yaml setlocal expandtab tabstop=2 shiftwidth=2

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
let g:go_list_autoclose = 1
let g:go_fmt_autosave = 1
let g:go_fmt_command = "goimports"
let g:go_fmt_fail_silently = 1
let g:go_def_reuse_buffer = 1
let g:go_echo_command_info = 1
let g:go_list_height = 10
let g:go_metalinter_enabled = ['vet', 'golint', 'errcheck', 'maligned', 'unconvert']

" run :GoBuild or :GoTestCompile based on the go file
function! s:build_go_files()
  let l:file = expand('%')
  if l:file =~# '^\f\+_test\.go$'
    call go#test#Test(0, 1)
  elseif l:file =~# '^\f\+\.go$'
    call go#cmd#Build(0)
  endif
endfunction

autocmd Filetype go command! -bang A call go#alternate#Switch(<bang>0, 'edit')
autocmd Filetype go command! -bang AV call go#alternate#Switch(<bang>0, 'vsplit')
autocmd Filetype go command! -bang AS call go#alternate#Switch(<bang>0, 'split')

autocmd FileType go nmap <silent> <leader>m :<C-u>call <SID>build_go_files()<CR>
autocmd FileType go nmap <Leader>r <Plug>(go-run)
autocmd FileType go nmap <Leader>i <Plug>(go-info)
autocmd FileType go nmap <Leader>d <Plug>(go-doc)
autocmd BufNewFile,BufRead *.go setlocal noexpandtab tabstop=4 shiftwidth=4

" -------------------------------
" fzf
" -------------------------------
nmap <Leader>b :Buffers<CR>
nmap <Leader>t :Files<CR>
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
" YouCompleteMe
" -------------------------------
set completeopt-=preview
let g:ycm_key_list_select_completion = ['<TAB>', '<C-j>']
let g:ycm_key_list_previous_completion = ['<C-k>']

" -------------------------------
" vim-polyglot
" -------------------------------
let g:polyglot_disabled = ['go']

" -------------------------------
" rust.vim
" -------------------------------
let g:rustfmt_autosave=1
autocmd BufRead,BufNewFile Cargo.toml,Cargo.lock,*.rs compiler cargo
