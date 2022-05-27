" Map leader to space, local leader to two spaces
nnoremap <Space> <nop>
let mapleader=" "
let maplocalleader="  "

" Initialize vim-plug
call plug#begin('~/.vim/plugged')

" core
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'chaoren/vim-wordmotion'
Plug 'qpkorr/vim-bufkill'
Plug 'jeetsukumaran/vim-filebeagle'
Plug '/usr/local/opt/fzf' 
Plug 'junegunn/fzf.vim'
Plug 'miyakogi/conoline.vim'
Plug 'jreybert/vimagit'
Plug 'MattesGroeger/vim-bookmarks'

" ui mods
Plug 'overcache/NeoSolarized'
Plug 'itchyny/lightline.vim'

" coc
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" project search
Plug 'mileszs/ack.vim'

call plug#end()

" -------------------------------
" Sensible defaults
" -------------------------------
set hidden
set shortmess+=c
set ignorecase
set smartcase
set splitbelow
set splitright
set autowrite
set wildmode=longest,list,full
set noshowmode
set undofile
set undodir=$HOME/.config/nvim/undo
set title
set scrolljump=8        " Scroll 8 lines at a time at bottom/top
set fillchars=fold:-,vert:│
set signcolumn=number
set number
set clipboard+=unnamedplus
set nocursorcolumn
set cmdheight=2
set updatetime=300


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
      \   'left': [['mode', 'paste'], [ 'cocstatus', 'relativepath', 'modified']],
      \   'right': [['lineinfo'], ['percent'], ['readonly']]
      \ },
      \ 'inactive': {
      \   'left': [ [ 'filename' ] ],
      \   'right': [ [ 'lineinfo' ], [ 'percent' ] ]
      \ },
	    \ 'component_function': {
	    \   'cocstatus': 'coc#status'
	    \ }
      \}

" -------------------------------
" vim-colors-solarized
"  - The only true colorscheme.
" -------------------------------
set background=dark
colorscheme NeoSolarized

" -------------------------------
" coc.nvim
" -------------------------------
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

autocmd CursorHold * silent call CocActionAsync('highlight')

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end


augroup formatting
  autocmd!
  autocmd BufWritePre *.py,*.go call CocAction('format')
augroup END


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
let g:fzf_layout = { 'down': '~20%' }
let g:fzf_preview_window = ['right:hidden', 'ctrl-/']

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
  autocmd FileType javascript,typescript,css,jsx,scss,json setlocal expandtab tabstop=2 shiftwidth=2
augroup END

" -------------------------------
" Markdown Syntax
" -------------------------------
augroup mdmode
  autocmd!
  autocmd FileType markdown setlocal spell spelllang=en_us expandtab tabstop=2 shiftwidth=2 autoindent colorcolumn=0 linebreak nonumber wrap textwidth=80
  autocmd FileType yaml setlocal expandtab tabstop=2 shiftwidth=2
augroup END

" -------------------------------
" Secret Environment Files
" -------------------------------
augroup secrets
  autocmd!
  autocmd BufRead,BufNewFile *.env.secret set filetype=sh
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
"  sql config
" -------------------------------
augroup sqlmode
  autocmd!
  autocmd FileType sql setlocal expandtab tabstop=4 shiftwidth=4
augroup END

" -------------------------------
" vim-magit
" -------------------------------
let g:magit_show_help=0
let g:magit_default_fold_level=2
let g:magit_default_sections = ['global_help', 'commit', 'staged', 'unstaged']

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
" vim-bookmarks
" -------------------------------
let g:bookmark_no_default_key_mappings=1

" -------------------------------
" vim-conoline
" -------------------------------
let g:conoline_auto_enable = 1
let g:conoline_use_colorscheme_default_normal=1
let g:conoline_use_colorscheme_default_insert=1


" -----------------------------------------------------
"  Convenience function to delete the buffer in the other window without
"  closing the window.
" -----------------------------------------------------
command! DeleteOtherBuffer call DeleteOtherBufferFn()
function! DeleteOtherBufferFn()
  wincmd p
  execute 'BD'
  wincmd p
endfunction

" -----------------------------------------------------
"  Convenience function to Populate the args list from the QuickFix list.
" -----------------------------------------------------
command! -nargs=0 -bar Qargs execute 'args ' . QuickfixFilenames()
function! QuickfixFilenames()
  " Building a hash ensures we get each buffer only once
  let buffer_numbers = {}
  for quickfix_item in getqflist()
    let buffer_numbers[quickfix_item['bufnr']] = bufname(quickfix_item['bufnr'])
  endfor
  return join(values(buffer_numbers))
endfunction

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

function! CompilationCheckQFList(lines)

  " Send all error lines into qf list
  let qflist = getqflist({'all': 1, 'lines': a:lines})
  let alllines = get(qflist, 'items', [])
  let errorlines = filter(alllines, 'get(v:val, "valid", 0) == 1')

  if len(errorlines) == len(g:seanpile_job_ctx.errorlines)
    return
  endif

  " Set the quickfix list
  call setqflist(errorlines, 'r')

  " Save new errorlines, and jump to first error if we have not yet jumped
  if !g:seanpile_job_ctx.jumped
    cfirst
  endif

  let g:seanpile_job_ctx.errorlines = errorlines
  let g:seanpile_job_ctx.jumped = 1

endfunction

" -----------------------------------------------------
"  Below is an ongoing experiment to simulate 'compilation' mode that is
"  present in emacs; commands are run in the preview window and output is
"  echoed to the preview window buffer
" -----------------------------------------------------
function! OnCallback(channel, line)

  if !exists("g:seanpile_job_ctx")
    return
  endif

  if ch_status(g:seanpile_job_ctx.job) == "buffered"
    return
  endif

  call add(g:seanpile_job_ctx.lines, a:line)
  call CompilationCheckQFList(g:seanpile_job_ctx.lines)

endfunction

function! OnCompilationFinish(job, code, event)

  " If job was killed early, then just return
  if a:code < 0
    return
  endif

  let bnum = g:seanpile_job_ctx.bnum

  " Do a final check of the output to determine if there are any
  " pending errorlines.
  if g:seanpile_job_ctx.loadqf
    let blines = getbufline(bnum, 3,  '$')
    call CompilationCheckQFList(blines)
  endif

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

  " Remove last job
  unlet g:seanpile_job_ctx

endfunction

function! OnCompilationOut(job_id, data, event) dict

  if !exists("g:seanpile_job_ctx")
    return
  endif

  let bnum = g:seanpile_job_ctx.bnum
  call setbufvar(bnum, '&modifiable', 1)
  call appendbufline(bnum, '$', a:data)
  call setbufvar(bnum, '&modifiable', 0)

endfunction

function! RunCompilation(command, description, loadqf)

  if exists("g:seanpile_job_ctx")
    call job_stop(g:seanpile_job_ctx.job, "kill")
  endif

  " Save all open files first
  execute 'wall'

  let bname = '\*compilation\*'
  let bnum = bufnr(bname)
  let maxwin = winnr('$')
  let cwid = -1
  if bnum >= 0
    for wnum in range(1, maxwin)
      if winbufnr(wnum) == bnum
        let cwid = win_getid(wnum)
      endif
    endfor
  endif

  if cwid >= 0 && maxwin > 1
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
  call appendbufline(bnum, 0, [
        \a:description . " started at " . strftime("%a %d %b %T")
  \])
  setlocal nomodifiable

  " Set error/success syntax matches
  syntax match Error /\v^[^:\ \t]+:\d+:(\d+:)?/
  syntax match Error /\v abnormally with code \d+/
  syntax match Statement /\v successfully /

  let opts = {
        \'on_exit': 'OnCompilationFinish',
        \'on_stdout': 'OnCompilationOut',
        \'on_stderr': 'OnCompilationOut'
  \}

  " Add callback to load entries into quickfix if required
  if a:loadqf
    call setqflist([], 'r')
    let opts.callback = 'OnCallback'
  endif

  " Kick off job
  let g:seanpile_job_ctx = {
    \'lines': [],
    \'errorlines': [],
    \'jumped': 0,
    \'loadqf': a:loadqf,
    \'bnum': bnum,
    \'job': jobstart(a:command, opts)
  \}

  " Switch to previous window
  wincmd p

endfunction

command! CompilationBuild call RunCompilation("./build.macosx", "Compilation", 1)
command! CompilationTest call RunCompilation("./test.macosx", "Testing", 1)
command! CompilationCheck call RunCompilation("./check.macosx", "Checking", 1)
command! CompilationRun call RunCompilation("./run.macosx", "Running", 0)

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

  if len(uniq(sort(columns))) <= 2
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


autocmd FileType python let b:coc_root_patterns = []
autocmd BufWritePre *.go :silent call CocAction('runCommand', 'editor.action.organizeImport')

" -------------------------------
" Keybindings
" -------------------------------
inoremap jj             <Esc>
" coc.nvim
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
inoremap <silent><expr> <c-space> coc#refresh()
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"

nmap <silent>     [g <Plug>(coc-diagnostic-prev)
nmap <silent>     ]g <Plug>(coc-diagnostic-next)
nmap <silent>     gd <Plug>(coc-definition)
nmap <silent>     gy <Plug>(coc-type-definition)
nmap <silent>     gi <Plug>(coc-implementation)
nmap <silent>     gr <Plug>(coc-references)
nmap <silent> K  :call <SID>show_documentation()<CR>
nmap <leader>cr  <Plug>(coc-rename)
nmap <leader>cf  <Plug>(coc-format)
nmap <leader>cc  <Plug>(coc-fix-current)
nmap <leader>co  :call CocActionAsync('runCommand', 'editor.action.organizeImport')<CR>
nmap <leader>cs  :call CocActionAsync('runCommand', 'editor.action.sortImport')<CR>
nmap <leader>c   <nop>

"  Vimrc editing
nnoremap <Leader>zz   :e ~/.config/nvim/init.vim<cr>
nnoremap <Leader>zr   :source ~/.config/nvim/init.vim<cr>
nnoremap <Leader>z    <nop>
"  Window Scrolling: / Motion
nnoremap <C-h>        <C-w>h
nnoremap <C-j>        <C-w>j
nnoremap <C-k>        <C-w>k
nnoremap <C-l>        <C-w>l
nnoremap <D-j>        <C-d>
nnoremap <D-k>        <C-u>
nnoremap <ScrollWheelUp>      <C-Y>
nnoremap <S-ScrollWheelUp>    <C-U>
nnoremap <ScrollWheelDown>    <C-E>
nnoremap <S-ScrollWheelDown>  <C-D>
"  Buffer/Window Management
nnoremap <Leader>s    :w<cr>
nnoremap <Leader>ww   :vsp<cr>
nnoremap <Leader>wp   :close<cr>
nnoremap <Leader>wo   :only<cr>
nnoremap <Leader>wx   :sp<cr>
nnoremap <Leader>w    <nop>
nnoremap <Leader>xo   :DeleteOtherBuffer<cr>
nnoremap <Leader>xx   :BD<cr>
nnoremap <Leader>xc   :let @" = expand("%")<cr>
nnoremap <Leader>x    <nop>
nnoremap <Leader><BS> :BD<cr>
tnoremap <C-g>        <C-W>N
tnoremap <C-x>        <C-W>N:BD!<cr>
"  File Navigation
nnoremap <Leader>/    :Ack<space>
nnoremap <Leader>b    :Buffers<cr>
nnoremap <Leader>r    :History<cr>
nnoremap <Leader>f    :Files<cr>
nmap     -            <Plug>FileBeagleOpenCurrentBufferDir
nmap     _            <Plug>FileBeagleOpenCurrentWorkingDir
"  Bookmark Support
nnoremap <Leader>tt   :BookmarkToggle<cr>
nnoremap <Leader>ta   :BookmarkShowAll<cr>
nnoremap <Leader>tr   :execute BookmarkShowAll()<bar>:Wall<bar>:bufdo bd<bar>Qargs<cr>
nnoremap <Leader>t    <nop>
"  Build / Error management
nnoremap <C-n>        :Cnext<cr>
nnoremap <C-p>        :Cprev<cr>
noremap  <Left>       :Cprev<cr>
noremap  <Right>      :Cnext<cr>
"noremap  <Up>         :cfirst<cr>
"noremap  <Down>       :clast<cr>
nnoremap <Leader>mm   :CompilationBuild<cr>
nnoremap <Leader>mt   :CompilationTest<cr>
nnoremap <Leader>mc   :CompilationCheck<cr>
nnoremap <Leader>mr   :CompilationRun<cr>
nnoremap <Leader>m    <nop>
