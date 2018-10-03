" Map leader to space, local leader to two spaces
nnoremap <Space> <nop>
let mapleader=" "
let maplocalleader="  "

" Initialize vim-plug
call plug#begin('~/.vim/plugged')

" core
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
Plug 'chaoren/vim-wordmotion'
Plug 'moll/vim-bbye'
Plug 'jeetsukumaran/vim-filebeagle'
Plug '/usr/local/opt/fzf' 
Plug 'junegunn/fzf.vim'

" ui mods
Plug 'altercation/vim-colors-solarized'
Plug 'itchyny/lightline.vim'

" project search
Plug 'mileszs/ack.vim'

" Minimalist Autocomplete
Plug 'lifepillar/vim-mucomplete'

" Code Formatters
Plug 'rhysd/vim-clang-format'
Plug 'mitermayer/vim-prettier'

" Language Plugins
Plug 'fatih/vim-go'

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
  autocmd ColorScheme * hi VertSplit ctermbg=NONE guibg=NONE

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
  set synmaxcol=120
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
      \   'left': [['mode', 'paste'], ['filename', 'modified']],
      \   'right': [['lineinfo'], ['percent'], ['readonly']]
      \ },
      \ 'inactive': {
      \   'left': [ [ 'filename' ] ],
      \   'right': [ [ 'lineinfo' ], [ 'percent' ] ]
      \ }
      \}

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
" fzf.vim
" -------------------------------
let g:fzf_layout = { 'down': '~10%' }

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
let go_gocode_propose_source = 0
let g:go_list_height = 10
let g:go_metalinter_enabled = ['vet', 'golint', 'errcheck', 'maligned', 'unconvert']
let g:go_highlight_functions = 0
let g:go_highlight_structs = 0
let g:go_highlight_interfaces = 0

augroup gomode
  autocmd!

  autocmd FileType go command! -bang A call go#alternate#Switch(<bang>0, 'edit')
  autocmd FileType go command! -bang AV call go#alternate#Switch(<bang>0, 'vsplit')
  autocmd FileType go command! -bang AS call go#alternate#Switch(<bang>0, 'split')

  autocmd FileType go nmap <buffer> <LocalLeader>r <Plug>(go-rename)
  autocmd FileType go nmap <buffer> <LocalLeader>d <Plug>(go-doc)
  autocmd FileType go nmap <buffer> <LocalLeader>D <Plug>(go-describe)
  autocmd FileType go nmap <buffer> <LocalLeader>I <Plug>(go-implements)
  autocmd FileType go nmap <buffer> <LocalLeader>l :GoReferrers<CR>
  autocmd FileType go nmap <buffer> <LocalLeader>i :GoImpl<CR>
  autocmd FileType go nmap <buffer> <LocalLeader>f :GoFill<CR>
  autocmd FileType go nmap <buffer> <LocalLeader>t :GoDecls<CR>
  autocmd FileType go nmap <buffer> <LocalLeader>T :GoDeclsDir<CR>
  autocmd FileType go setlocal tabstop=4 shiftwidth=4
  autocmd FileType go setlocal formatoptions=cjrql
augroup END

" -------------------------------
"  fzf config
" -------------------------------
augroup fzf
  autocmd!

  " Send ctrl-j/ctrl-k to fzf instead of trying to jump between windows
  autocmd FileType fzf tnoremap <expr> <buffer> <C-j> SendToTerm("\<c-j>")
  autocmd FileType fzf tnoremap <expr> <buffer> <C-k> SendToTerm("\<c-k>")
augroup END

function! SendToTerm(what)
  call term_sendkeys('', a:what)
  return ''
endfunc

" -------------------------------
" FileBeagle
" -------------------------------
let g:loaded_netrw       = 1
let g:loaded_netrwPlugin = 1
let g:filebeagle_suppress_keymaps = 1

" -------------------------------
" bufkill
" -------------------------------
let g:BufKillCreateMappings = 0

" -------------------------------
" bufkill
" -------------------------------
let g:terraform_fmt_on_save = 1


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
let g:polyglot_disabled = ['go', 'terraform']

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
nnoremap <Leader>wp   :b#<cr>
nnoremap <Leader>w    <nop>
nnoremap <Leader>xo   :execute 'Bdelete' winbufnr(winnr('#'))<cr>
nnoremap <Leader>xx   :Bdelete<cr>
nnoremap <Leader>x    <nop>
nnoremap <Leader><BS> :Bdelete<cr>
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
"  Build / Error management
nnoremap <C-n>        :Cnext<cr>
nnoremap <C-p>        :Cprev<cr>
noremap  <Left>       :Cprev<cr>
noremap  <Right>      :Cnext<cr>
noremap  <Up>         :cfirst<cr>
noremap  <Down>       :clast<cr>
nnoremap <Leader>m    :CompilationBuild<cr>
nnoremap <Leader>1    :CompilationTest<cr>

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
function! OnBuildFinish(job, code)
  call OnCompilationFinish(a:job, a:code, &errorformat, 1)
endfunction

function! OnTestFinish(job, code)
  call OnCompilationFinish(a:job, a:code, &errorformat, 1)
endfunction

function! OnCompilationFinish(job, code, efm, loadqf)

  " If job was killed early, then just return
  if a:code < 0
    return
  endif

  let bnum = bufnr('\*compilation\*')
  if bnum < 0
    return
  endif

  " Output of job, excluding header
	let blines = getbufline(bnum, 3,  '$')

  call setbufvar(bnum, '&modifiable', 1)
  if a:code == 0 
    call appendbufline(bnum, '$', [
          \"",
          \"Finished successfully at " . strftime("%a %d %b %T")
          \])
  else
    call appendbufline(bnum, '$', [
          \"",
          \"Finished abnormally with code " . a:code . " at " . strftime("%a %d %b %T")
          \])
  endif
  call setbufvar(bnum, '&modifiable', 0)

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
    let qflist = getqflist({'all': 1, 'efm': a:efm, 'lines': blines})
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

  " Save all open files first
  execute 'wall'

  let bname = '\*compilation\*'
  let bnum = bufnr(bname)
  let cwid = -1
  if bnum >= 0
    for wnum in range(1, winnr('$'))
      if winbufnr(wnum) == bnum
        let cwid = win_getid(wnum)
      endif
    endfor
  endif

  if cwid >= 0
    " Go to existing window with compilation buffer if it exists
    call win_gotoid(cwid)
  else
    " Otherwise, open up a new vertical split
    execute 'silent vsplit'
  endif

  execute 'silent edit' bname
  setlocal filetype=compilation
  setlocal buftype=nofile
  setlocal nobuflisted
  setlocal nonumber
  let bnum = bufnr(bname)

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

  " Kick off job
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

" DualPaneLayoutManager is called on new windows to maintain a dual-pane window
" layout.
function! DualPaneLayoutManager()

  " Allow horizontal splits to occur
  let screenpos = win_screenpos(winnr())
  if screenpos[0] > 1
    return
  endif

  let tabnr = tabpagenr()
  let nwin = tabpagewinnr(tabnr, '$')

  " If we only have two columns, allow split to continue
  let columns = []
  for cwin in range(1, nwin)
    let col = win_screenpos(cwin)[1]
    call add(columns, win_screenpos(cwin)[1])
  endfor

  if len(uniq(columns)) <= 2
    return
  endif

  " If we only have two windows, don't do anything
  let lwin = nwin
  if lwin <= 2
    return
  endif

  " Go through existing windows and see if there are any horizontal splits
  let cwin = 1
  while cwin <= lwin
    let row = win_screenpos(cwin)[0]
    if row > 1
      execute cwin . 'quit'
      let lwin -= 1
    else
      let cwin += 1
    endif
  endwhile

  " Now rotate windows and close the one after the just recently opened window
  wincmd r
  let cwin = winnr()
  let nwin = cwin + 1
  if nwin > lwin
    let nwin = 1
  endif
  execute nwin . 'quit'
endfunction

augroup windowlayout
  autocmd!
  autocmd WinNew * call DualPaneLayoutManager()
augroup END
