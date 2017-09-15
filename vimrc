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

" NERD Tree
Plugin 'scrooloose/nerdtree'

" CtrlP
Plugin 'kien/ctrlp.vim'

" Modify word motions to be CamelCase/snake_case aware
Plugin 'chaoren/vim-wordmotion'

" Autocomplete
Plugin 'ervandew/supertab'

" Quickfix/Buffer Fixes
Plugin 'qpkorr/vim-bufkill'
Plugin 'milkypostman/vim-togglelist'

" User Interface Mods
Plugin 'altercation/vim-colors-solarized'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

" Better grepping
Plugin 'mileszs/ack.vim'

" Golang
Plugin 'fatih/vim-go'

" Code formatter
Plugin 'rhysd/vim-clang-format'

" JavaScript
Plugin 'pangloss/vim-javascript'
Plugin 'mitermayer/vim-prettier'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
syntax on

set hidden
set belloff=all
set number
set scrolloff=999
set splitbelow
set splitright
set equalalways
set nobackup
set noswapfile
set autowrite
set wildmode=longest,list,full
set wildmenu
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
nnoremap <Leader>n1 :sp #1<CR>:wincmd p<CR>
nnoremap <Leader>n2 :sp #2<CR>:wincmd p<CR>
nnoremap <Leader>n3 :sp #3<CR>:wincmd p<CR>
nnoremap <Leader>n4 :sp #4<CR>:wincmd p<CR>
nnoremap <Leader>n5 :sp #5<CR>:wincmd p<CR>
nnoremap <Leader>n6 :sp #6<CR>:wincmd p<CR>
nnoremap <Leader>n7 :sp #7<CR>:wincmd p<CR>
nnoremap <Leader>n8 :sp #8<CR>:wincmd p<CR>
nnoremap <Leader>n9 :sp #9<CR>:wincmd p<CR>

" Easy Navigation of Quickfix list
noremap <Left> :cprev<CR>
noremap <Right> :cnext<CR>
noremap <Up> :cfirst<CR>
noremap <Down> :clast<CR>

" Close a buffer:  <Leader> Backspace
" Close a window:  <Leader> w
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
    let g:airline_powerline_fonts = 1
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
nnoremap <Leader>k :Ack<CR>:wincmd h<CR>
let g:ack_use_cword_for_empty_search = 1
if executable('ag')
    let g:ackprg = 'ag --vimgrep'
endif

" -------------------------------
" Airline
" -------------------------------
let g:airline_theme='solarized'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_idx_mode = 1
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline#extensions#tabline#fnamemod = ':p:t'
let g:airline#extensions#tabline#show_splits = 0

nnoremap <Leader>b :ls<CR>:b<Space>
nmap <leader>1 <Plug>AirlineSelectTab1
nmap <leader>2 <Plug>AirlineSelectTab2
nmap <leader>3 <Plug>AirlineSelectTab3
nmap <leader>4 <Plug>AirlineSelectTab4
nmap <leader>5 <Plug>AirlineSelectTab5
nmap <leader>6 <Plug>AirlineSelectTab6
nmap <leader>7 <Plug>AirlineSelectTab7
nmap <leader>8 <Plug>AirlineSelectTab8
nmap <leader>9 <Plug>AirlineSelectTab9
nmap <leader>- <Plug>AirlineSelectPrevTab
nmap <leader>= <Plug>AirlineSelectNextTab

let g:airline#extensions#tabline#buffer_idx_format = {
        \ '0': '0 ',
        \ '1': '1 ',
        \ '2': '2 ',
        \ '3': '3 ',
        \ '4': '4 ',
        \ '5': '5 ',
        \ '6': '6 ',
        \ '7': '7 ',
        \ '8': '8 ',
        \ '9': '9 '
        \}

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
" Turn off automappings for vim-bufkill
" -------------------------------
let g:BufKillCreateMappings=0

" -------------------------------
" Quickfix Configuration
" -------------------------------
nmap <script> <silent> <Leader>c :call ToggleQuickfixList()<CR>
let g:toggle_list_copen_command="vertical copen 90"
augroup quickfix
    autocmd!
    autocmd FileType qf set nobuflisted
augroup END

" -------------------------------
" NERDTree
" -------------------------------
nnoremap <Leader>/ :NERDTreeToggle<CR>

" -------------------------------
" Golang Config
" -------------------------------
let g:go_list_type = "quickfix"
let g:go_list_autoclose = 1
let g:go_fmt_autosave = 1
let g:go_fmt_command = "goimports"
let g:go_fmt_fail_silently = 1
let g:go_metalinter_autosave = 0
let g:go_metalinter_autosave_enabled = ['vet', 'golint']
let g:go_def_reuse_buffer = 1
let g:go_echo_command_info = 0
let g:go_list_height = 10
autocmd FileType go let g:airline_section_c = '%t %{go#statusline#Show()}'

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

autocmd FileType go nnoremap <leader>m :<C-u>call <SID>build_go_files()<CR>
autocmd FileType go nnoremap <Leader>r :GoRun<CR>
autocmd FileType go nnoremap <Leader>t :GoTest ./...<CR>
autocmd FileType go nnoremap <Leader>i :GoImports<CR>
autocmd FileType go nnoremap <Leader>i :GoInfo<CR>
autocmd BufNewFile,BufRead *.go setlocal noexpandtab tabstop=4 shiftwidth=4

" -------------------------------
" CtrlP
" -------------------------------
let g:ctrlp_map = '<Leader>p'
let g:ctrlp_show_hidden=1
let g:ctrlp_use_caching=0
let g:ctrlp_clear_cache_on_exit=1
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files']
