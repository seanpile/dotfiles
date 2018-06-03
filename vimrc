" Map leader to space, local leader to two spaces
nnoremap <Space> <nop>
let mapleader=" "
let maplocalleader="  "

" Initialize vim-plug
call plug#begin('~/.vim/plugged')

" core
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
Plug 'chaoren/vim-wordmotion'
Plug 'qpkorr/vim-bufkill'
Plug 'jeetsukumaran/vim-filebeagle'
Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'

" ui mods
Plug 'altercation/vim-colors-solarized'
Plug 'itchyny/lightline.vim'

" project search
Plug 'mileszs/ack.vim'

" Minimalist Autocomplete
Plug 'lifepillar/vim-mucomplete'

" Syntax/Indent for all languages
Plug 'sheerun/vim-polyglot'

" Code Formatters
Plug 'rhysd/vim-clang-format'
Plug 'mitermayer/vim-prettier'

" Language Plugins
Plug 'fatih/vim-go', { 'tag': 'v1.17' }

call plug#end()


" -------------------------------
" Sensible defaults
" -------------------------------
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
set cursorline
set undofile
set autoread
set title
set scrolljump=8        " Scroll 8 lines at a time at bottom/top
set undodir=$HOME/.vim/undo

" Make the vertical split separator less ugly
set fillchars+=vert:│
autocmd! ColorScheme * hi VertSplit ctermbg=NONE guibg=NONE

" Jump to last position in buffer when opening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" -------------------------------
" Speed related optimizations; on terminal, these are all needed
" otherwise performance bogs down way too much.
" -------------------------------
if !has('gui_running')
  let loaded_matchparen=1 " Don't load matchit.vim (paren/bracket matching)
  set noshowmatch         " Don't match parentheses/brackets
  set nocursorcolumn      " Don't paint cursor column
  set nocursorline        " Don't paint cursor line
endif

" -------------------------------
" Lightline
"  - Minimal modeline
" -------------------------------
let g:lightline = {
      \ 'colorscheme': 'solarized',
      \ 'active': {
      \   'left': [['mode', 'paste'], ['filename', 'gitversion', 'modified']],
      \   'right': [['lineinfo'], ['percent'], ['readonly']]
      \ },
      \ 'inactive': {
      \   'left': [ [ 'filename', 'gitversion' ] ],
      \   'right': [ [ 'lineinfo' ], [ 'percent' ] ]
      \ },
      \ 'component_function': {
      \   'gitversion': 'LightLineGitversion'
      \ }
      \}

function! LightLineGitversion()
  let fullname = expand('%')
  let gitversion = ''
  if fullname =~? 'fugitive://.*/\.git//0/.*'
      let gitversion = 'git index'
  elseif fullname =~? 'fugitive://.*/\.git//2/.*'
      let gitversion = 'git target'
  elseif fullname =~? 'fugitive://.*/\.git//3/.*'
      let gitversion = 'git merge'
  elseif &diff == 1
      let gitversion = 'working copy'
  endif
  return gitversion
endfunction

" -------------------------------
" vim-colors-solarized
"  - The only true colorscheme.
" -------------------------------
set background=dark
colorscheme solarized

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

  " Set font (system dependant)
  set encoding=utf8
  if has("gui_win32")
    set guifont=Consolas:h10:cANSI
  else
    set guifont=Knack\ Regular\ Nerd\ Font\ Complete:h12
  endif
endif

" -------------------------------
" Ack.vim
"  - Try to use `rg` / `ag` if they are available
" -------------------------------
let g:ack_use_cword_for_empty_search = 1
if executable('rg')
  let g:ackprg = 'rg --vimgrep --no-heading'
elseif executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

" -------------------------------
" clang-format
" -------------------------------
if executable('clang-format')
  let g:clang_format#detect_style_file=1
  let g:clang_format#auto_formatexpr=1
endif

" -------------------------------
" vimrc specific settings
" -------------------------------
augroup vimmode
  autocmd!
  autocmd FileType vim setlocal expandtab tabstop=2 shiftwidth=2
augroup END

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
  autocmd FileType javascript,css,jsx,scss,json silent! unnmap <LocalLeader>
  autocmd FileType javascript,css,jsx,scss,json nmap <LocalLeader>f :Prettier<cr>
  autocmd FileType javascript,css,jsx,scss,json setlocal expandtab tabstop=2 shiftwidth=2
augroup END

" -------------------------------
" Markdown Syntax
" -------------------------------
augroup mdmode
  autocmd!
  autocmd BufWritePre *.md Prettier
  autocmd FileType markdown setlocal expandtab tabstop=2 shiftwidth=2 autoindent colorcolumn=0 linebreak nonumber wrap textwidth=100
  autocmd FileType markdown let g:prettier#config#parser='markdown'
  autocmd FileType markdown let g:prettier#config#prose_wrap='always'
  autocmd FileType yaml setlocal expandtab tabstop=2 shiftwidth=2
augroup END

" -------------------------------
" Quickfix Configuration
"  - Create commands that will cycle at the end of the error list
"  - Don't list qf in buffer list
"  - Ensure qf always opens up on bottom of screen
" -------------------------------
command! Cnext try | cnext | catch | cfirst | catch | endtry
command! Cprev try | cprev | catch | clast | catch | endtry
augroup quickfix
  autocmd!
  autocmd FileType qf set nobuflisted
  autocmd FileType qf wincmd J
augroup END

" -------------------------------
" Golang Config
" -------------------------------
let g:go_list_type = "quickfix"
let g:go_fmt_autosave = 1
let g:go_fmt_command = "goimports"
let g:go_fmt_fail_silently = 1
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

  autocmd FileType go silent! nunmap <LocalLeader>
  autocmd FileType go nmap <LocalLeader>r <Plug>(go-run)
  autocmd FileType go nmap <LocalLeader>i <Plug>(go-info)
  autocmd FileType go nmap <LocalLeader>d <Plug>(go-doc)
  autocmd FileType go nmap <LocalLeader>t :GoDecls<CR>
  autocmd FileType go nmap <LocalLeader>T :GoDeclsDir<CR>
  autocmd FileType go setlocal tabstop=4 shiftwidth=4
  autocmd FileType go setlocal formatoptions=jrql
augroup END

" -------------------------------
" fzf
" -------------------------------
let $FZF_DEFAULT_COMMAND="rg --files --hidden -g \!.git -g \!vendor/"

" -------------------------------
" FileBeagle
" -------------------------------
let g:filebeagle_suppress_keymaps = 1

" -------------------------------
" bufkill
" -------------------------------
let g:BufKillCreateMappings = 0

" -------------------------------
" MUComplete
" -------------------------------
set completeopt=menuone,noselect
let g:mucomplete#chains = {}
let g:mucomplete#chains.sql = ['file']
let g:mucomplete#chains.go = ['path', 'omni', 'keyn']

" -------------------------------
" vim-polyglot
" -------------------------------
let g:polyglot_disabled = ['go']

" -------------------------------
" Keybindings
" -------------------------------
"  Vimrc editing
nnoremap <Leader>zz   :e $MYVIMRC<cr>
nnoremap <Leader>zr   :source $MYVIMRC<cr>
nnoremap <Leader>z    <nop>
"  Unified Window Switching (in vim + terminal mode)
nnoremap <C-h>        <C-w>h
nnoremap <C-j>        <C-w>j
nnoremap <C-k>        <C-w>k
nnoremap <C-l>        <C-w>l
tnoremap <C-h>        <C-W>h
tnoremap <C-j>        <C-W>j
tnoremap <C-k>        <C-W>k
tnoremap <C-l>        <C-W>l
"  Buffer/Window Management
nnoremap <Leader>s    :w<cr>
nnoremap <Leader>wq   :close<cr>
nnoremap <Leader>wo   :only<cr>
nnoremap <Leader>ww   :vsp<cr>
nnoremap <Leader>wx   :sp<cr>
nnoremap <Leader>w    <nop>
nnoremap <Leader><BS> :BD<cr>
nnoremap <C-p>        :b#<cr>
tnoremap <C-p>        <C-W>:b#<cr>
tnoremap <C-g>        <C-W>N
tnoremap <C-x>        <C-W>N:BD!<cr>
"  Terminal Support
nnoremap <Leader>tt   :call OpenExistingTerminal()<cr>
nnoremap <Leader>tn   :call OpenNewTerminal()<cr>
nnoremap <Leader>t    <nop>
"  File Navigation
nnoremap <Leader>/    :Ack<space>
nnoremap <Leader>b    :Buffers<cr>
nnoremap <Leader>r    :History<cr>
nnoremap <Leader>f    :Files<cr>
nmap     -            <Plug>FileBeagleOpenCurrentBufferDir
nmap     _            <Plug>FileBeagleOpenCurrentWorkingDir
"  Build / Error management
noremap  <Left>       :Cprev<cr>
noremap  <Right>      :Cnext<cr>
noremap  <Up>         :cfirst<cr>
noremap  <Down>       :clast<cr>

function! OpenExistingTerminal()
  let termbuffers = filter(range(1, bufnr('$')), 
        \'bufexists(v:val) && getbufvar(v:val, "&buftype", "") == "terminal"')
  if len(termbuffers) > 0
      execute 'buffer' termbuffers[0]
      execute 'startinsert!'
      return
  endif

  " Fallback to creating a new terminal
  call OpenNewTerminal()
endfunction

function! OpenNewTerminal()
  execute 'term ++curwin ++noclose ++kill=term'
endfunction

function! OnCompilationFinish(job, code)

  let bnum = SwitchToModeWindow('*compilation*')
  let binfo = getbufinfo(bnum)[0]
  let blines = get(binfo, 'lnum')

  " TODO: Highlight error in `Compilation finished tag...`
  " TODO: Highlight errors in buffer

  setlocal modifiable
  call append(blines, [
        \"",
        \"Finished at " . strftime("%a %d %b %T")
        \])
  setlocal nomodifiable

  " Switch back to previous window
  wincmd p

  " If there is an error building, load the buffer into the quickfix list,
  " skipping the header/footer.
  if a:code == 0
    " Clear quickfix list
    call setqflist([], 'r')
  else
    " Send all error lines into qf list
    let qflist = getqflist({'lines': getbufline(bnum, 2, blines)})
    let alllines = get(qflist, 'items', [])
    let errorlines = filter(alllines, 'get(v:val, "valid", 0) == 1')
    call setqflist(errorlines, 'r')
    cfirst
  endif

endfunction

function! SwitchToModeWindow(mode)

  " Find existing window that has the mode window (if any exists)
  for wnum in range(1, winnr('$'))
    let bnum = winbufnr(wnum)
    if a:mode == bufname(bnum)
      call win_gotoid(win_getid(wnum))
      return bnum
    endif
  endfor


  " Pick a suitable window to jump to to display the compilation buffer
  " TODO: Better selection process
  let curwin = winnr()
  let found = 0
  for wnum in range(1, winnr('$'))
    if wnum != curwin
      let found = wnum
      call win_gotoid(win_getid(wnum))
      break
    endif
  endfor

  " If no window was found, it probably means there was only 1 window
  " Split a new one.
  if found == 0
    vsplit
  endif

  " Bring up buffer and set read only properties on it
  execute 'edit' a:mode
  setlocal filetype=compilation-mode
  setlocal buftype=nofile
  setlocal nomodifiable
  setlocal nonumber
  return bufnr(a:mode)

endfunction

function! RunCompilation(command, description)

  " Switch to compilation window and return buffer number
  let bnum = SwitchToModeWindow('*compilation*')

  " Show compilation header
  setlocal modifiable
  normal! ggdG
  call append(0, [
        \a:description . " started at " . strftime("%a %d %b %T")
        \])
  setlocal nomodifiable

  call job_start(a:command, {
        \'exit_cb': 'OnCompilationFinish',
        \'out_modifiable': 0,
        \'out_io': 'buffer',
        \'out_buf': bnum,
        \'err_modifiable': 0,
        \'err_io': 'buffer',
        \'err_buf': bnum
        \})

  " Switch to previous window
  wincmd p

endfunction

nnoremap <Leader>m  :call RunCompilation("./build.macosx", "Compilation")<cr>

autocmd WinEnter * if &previewwindow && winnr() > 1 | wincmd H | endif
