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
Plug 'moll/vim-bbye'
Plug 'jeetsukumaran/vim-filebeagle'
Plug '/usr/local/opt/fzf' | Plug 'junegunn/fzf.vim'
Plug 'vim-scripts/ingo-library'

" ui mods
Plug 'altercation/vim-colors-solarized'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/goyo.vim'

" project search
Plug 'mileszs/ack.vim'

" Minimalist Autocomplete
Plug 'lifepillar/vim-mucomplete'

" Code Formatters
Plug 'rhysd/vim-clang-format'
Plug 'mitermayer/vim-prettier'

" Language Plugins
Plug 'fatih/vim-go', { 'branch': 'gocode-change' }

" Syntax/Indent for all languages
Plug 'sheerun/vim-polyglot'

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
set ttyfast
set cursorline
set undofile
set autoread
set title
set scrolljump=8        " Scroll 8 lines at a time at bottom/top
set undodir=$HOME/.vim/undo
set fillchars=fold:-,vert:│

augroup uistuff
  autocmd!
  
  " Remove ugly background on vertical split bars
  autocmd! ColorScheme * hi VertSplit ctermbg=NONE guibg=NONE

  " Disable syntax highlighting for vimdiff buffers
  autocmd BufEnter * if &diff | execute "ownsyntax off" | endif

  " Jump to last position in buffer when opening a file
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
augroup END

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
" Preview Window
" - Force Preview window to take up half the vertical space on the right
" -------------------------------
augroup preview
  autocmd!
  autocmd WinEnter * if &previewwindow | wincmd L | endif
  autocmd FileType compilation nmap <buffer> q :close<cr>
augroup END

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
    set guifont=Hack\ Regular\ Nerd\ Font\ Complete:h12
  endif

  " Green Cursor, maximum visibility
  highlight Cursor guibg=Green

  " Solarized Terminal Colors
  let g:terminal_ansi_colors = [
        \ '#073642',
        \ '#e9523e',
        \ '#859900',
        \ '#b58900',
        \ '#268bd2',
        \ '#d33682',
        \ '#2aa198',
        \ '#eee8d5',
        \ '#002b36',
        \ '#cb4b16',
        \ '#586e75',
        \ '#657b83',
        \ '#839496',
        \ '#6c71c4',
        \ '#93a1a1',
        \ '#fdf6e3',
        \ ]

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
  autocmd FileType c,cpp,objc setlocal formatoptions=cjrql
  autocmd FileType c,cpp,objc setlocal expandtab tabstop=4 shiftwidth=4
augroup END

" -------------------------------
" Javascript Settings
" -------------------------------
augroup jsmode
  autocmd!
  autocmd FileType javascript,css,jsx,scss,json nmap <buffer> <LocalLeader>f :Prettier<cr>
  autocmd FileType javascript,css,jsx,scss,json setlocal expandtab tabstop=2 shiftwidth=2
augroup END

" -------------------------------
" Markdown Syntax
" -------------------------------
augroup mdmode
  autocmd!
  autocmd BufWritePre *.md Prettier
  autocmd FileType markdown setlocal expandtab tabstop=2 shiftwidth=2 autoindent colorcolumn=0 linebreak nonumber wrap textwidth=80
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

  autocmd FileType go nmap <buffer> <LocalLeader>r <Plug>(go-rename)
  autocmd FileType go nmap <buffer> <LocalLeader>i <Plug>(go-import)
  autocmd FileType go nmap <buffer> <LocalLeader>d <Plug>(go-doc)
  autocmd FileType go nmap <buffer> <LocalLeader>D <Plug>(go-describe)
  autocmd FileType go nmap <buffer> <LocalLeader>f :GoFill<CR>
  autocmd FileType go nmap <buffer> <LocalLeader>t :GoDecls<CR>
  autocmd FileType go nmap <buffer> <LocalLeader>T :GoDeclsDir<CR>
  autocmd FileType go setlocal tabstop=4 shiftwidth=4
  autocmd FileType go setlocal formatoptions=cjrqla
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
" goyo
" -------------------------------
let g:goyo_width=100

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
nnoremap <Leader>xo   :DeleteOtherBuffer<cr>
nnoremap <Leader>xx   :DeleteThisBuffer<cr>
nnoremap <Leader>x    <nop>
nnoremap <Leader><BS> :DeleteThisBuffer<cr>
nnoremap <C-p>        :b#<cr>
tnoremap <C-p>        <C-W>N:b#<cr>
tnoremap <C-g>        <C-W>N
tnoremap <C-x>        <C-W>N:Bdelete!<cr>
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
"  Git Support
nnoremap <Leader>gg   :call SetupPreviewWindow()<cr>:Gstatus<cr>
nnoremap <Leader>gp   :only<cr>:Git! pull<cr>
nnoremap <Leader>gP   :only<cr>:G
"  Build / Error management
noremap  <Left>       :Cprev<cr>
noremap  <Right>      :Cnext<cr>
noremap  <Up>         :cfirst<cr>
noremap  <Down>       :clast<cr>


" -----------------------------------------------------
"  Convenience functions for interacting with terminal;
"  - OpenExistingTerminal searches for an existing buffer of type `terminal`
"  and attempts to re-use that buffer
"  - OpenNewTerminal creates a new terminal buffer with some sensible default
"  options
" -----------------------------------------------------

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


" -----------------------------------------------------
"  Below is an ongoing experiment to simulate 'compilation' mode that is
"  present in emacs; commands are run in the preview window and output is
"  echoed to the preview window buffer
" -----------------------------------------------------
function! SetupPreviewWindow()
  if winnr('$') == 1
    vsplit
  elseif winnr('$') > 2 && !&previewwindow
    only
  endif
endfunction

function! DeleteThisBuffer()
  execute 'Bdelete'

  " Unset preview window if current window
  if &previewwindow
    let &previewwindow = 0
  endif
endfunction

function! DeleteOtherBuffer()
  let otherwin = winnr('#')

  " Delete buffer in other window
  execute "Bdelete" winbufnr(otherwin)

  " Now check if other window is a preview window; if so, unset it
  if getwinvar(otherwin, '&previewwindow', 0)
    call setwinvar(otherwin, '&previewwindow', 0)
  endif
endfunction

command! DeleteThisBuffer call DeleteThisBuffer();
command! DeleteOtherBuffer call DeleteOtherBuffer();

function! OnBuildFinish(job, code)
  call OnCompilationFinish(a:job, a:code, &errorformat, 1)
endfunction

function! OnTestFinish(job, code)
  let testformat='%f:%l%m'
  call OnCompilationFinish(a:job, a:code, testformat, 1)
endfunction

function! OnCompilationFinish(job, code, efm, loadqf)

  " If job was killed early, then just return
  if a:code < 0
    return
  endif

  let bnum = bufnr('*compilation*')
  if bnum < 0
    return
  endif

  " Open Compilation Buffer in preview window
  call ingo#window#preview#GotoPreview()

  " If we don't currently have our compilation buffer loaded, load it now
  if winbufnr(0) != bnum
    execute bnum 'buffer'
  endif

  let binfo = getbufinfo(bnum)[0]
  let blines = get(binfo, 'lnum')

  " TODO: Highlight error in `Compilation finished tag...`
  " TODO: Highlight errors in buffer

  setlocal modifiable
  if a:code == 0 
    call append(blines, [
          \"",
          \"Finished successfully at " . strftime("%a %d %b %T")
          \])
  else
    call append(blines, [
          \"",
          \"Finished abnormally with code " . a:code . " at " . strftime("%a %d %b %T")
          \])
  endif
  setlocal nomodifiable

  " Switch back to previous window
  wincmd p

  if !a:loadqf
    return
  endif

  " If there is an error building, load the buffer into the quickfix list,
  " skipping the header/footer.
  if a:code == 0
    " Clear quickfix list
    call setqflist([], 'r')
  else
    " Send all error lines into qf list
    let qflist = getqflist({'all': 1, 'efm': a:efm, 'lines': getbufline(bnum, 3, blines)})
    echom string(qflist)
    let alllines = get(qflist, 'items', [])
    let errorlines = filter(alllines, 'get(v:val, "valid", 0) == 1')
    call setqflist(errorlines, 'r')
    cfirst
  endif

endfunction

function! RunCompilation(command, description, onFinish)

  if exists("g:seanpile_last_job")
    call job_stop(g:seanpile_last_job, "kill")
  endif

  if !&previewwindow && winnr('$') > 1
    only
  endif

  " Save all open files first
  execute 'wall'

  " Open Compilation Buffer in preview window
  call ingo#window#preview#OpenPreview()

  let bname = '*compilation*'
  execute 'silent edit' bname
  setlocal filetype=compilation
  setlocal buftype=nofile
  setlocal nomodifiable
  setlocal nobuflisted
  setlocal nonumber
  let bnum = bufnr(bname)

  " Show compilation header
  setlocal modifiable
  silent! normal! ggdG
  call append(0, [
        \a:description . " started at " . strftime("%a %d %b %T")
        \])
  setlocal nomodifiable

  " Set error/success syntax matches
  syntax match Error /\v^[^:\ \t]+:\d+:(\d+:)?/
  syntax match Error /\v abnormally with code \d+/
  syntax match Statement /\v successfully /

  let g:seanpile_last_job = job_start(a:command, {
        \'exit_cb': a:onFinish,
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

command! CompilationBuild call RunCompilation("./build.macosx", "Compilation", "OnBuildFinish")
command! CompilationTest call RunCompilation("./test.macosx", "Testing", "OnTestFinish")

nnoremap <Leader>m  :CompilationBuild<cr>
nnoremap <Leader>1  :CompilationTest<cr>
