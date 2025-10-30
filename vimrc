" An example for a vimrc file.
"
" Original Maintainer:   Bram Moolenaar <Bram@vim.org> (RIP)
" Customized by: gopeterjun@naver.com and OpenAI Codex
" Last change:  Wed 24 Sep 2025
"
" To use it, copy it to
"     for Unix and OS/2:  ~/.vimrc
"             for Amiga:  s:.vimrc
"  for MS-DOS and Win32:  $VIM\_vimrc
"           for OpenVMS:  sys$login:.vimrc

" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("vms")
  set nobackup          " do not keep a backup file, use versions instead
else
  set backup            " keep a backup file (restore to previous version)
  set undofile          " keep an undo file (undo changes after closing)
  set undodir=~/tmp     " path for undo files
  set backupdir=~/tmp   " path for backup files
  set directory=~/tmp   " path for swap files
endif
set history=50          " keep 50 lines of command line history
set ruler               " show the cursor position all the time
set showcmd             " display incomplete commands
set incsearch           " do incremental searching
set cursorline          " highlight current line (for Ghostty)

" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
" let &guioptions = substitute(&guioptions, "t", "", "g")

" Don't use Ex mode, use Q for formatting
map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
" And specify colorscheme (aka 'colo')
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
  "default colors defined in '/usr/share/vim/vimXX/colors/'
  "colo elflord
  " Prefer Dracula if installed as a Vim package; fall back to Shine.
  if isdirectory(expand('~/.vim/pack/themes/start/dracula'))
    silent! colorscheme dracula
  else
    colorscheme shine
  endif
  "colo evening
endif

" Tab settings
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab

" Set path for plugins
" The following is the default plugins path for vim on Arch
"set packpath=/usr/share/vim/vimfiles/plugin/
" On Ubuntu, the following needs to be uncommented (Run Time Path)
" When using the system packages you can simply install `vim-ale`.
" ALE discovers available linters automatically by inspecting what is
" available on your $PATH.

set rtp+=/usr/share/vim/vimfiles/plugin/  "runtime path for Fedora
"set rtp+=/usr/share/vim/addons/plugin    "runtime path for ?

" ---------------------------------------------------------------------------
" Asynchronous Linting Engine (ALE) settings – replacement for the deprecated
" Syntastic plugin.  ALE performs linting and fixing in the background and is
" the de-facto standard for modern Vim/Neovim setups.

" 1.  Runtime path – most distributions place ALE in the generic Vim plugin
"     directory (e.g. /usr/share/vim/vimfiles).  If you keep using the system
"     packages, the earlier `set rtp+=` line already covers this.  The extra
"     check keeps Vim quiet when ALE is not installed.

if empty(filter(split(&rtp, ','), 'v:val =~# "ale"'))
  " Uncomment the next line when ALE lives in a non-standard location.
  " set rtp+=~/path/to/ale
endif

" 2.  Basic configuration.

" Enable linting on text change as well as when leaving insert mode.
let g:ale_lint_on_text_changed = 'always'
let g:ale_lint_on_insert_leave = 1

" Use the location list, not the quickfix list, so the errors are per-buffer.
let g:ale_set_loclist = 1
let g:ale_set_quickfix = 0

" Show virtual text (`signcolumn`) symbols.
let g:ale_sign_error   = '✗'
let g:ale_sign_warning = '△'

" Fixers – run :ALEFix (or automatically on save when turned on).
let g:ale_fixers = {
      \   '*': ['remove_trailing_lines', 'trim_whitespace'],
      \   'python': ['isort', 'black'],
      \ }

" Enable auto-fixing on write for the languages listed above.
let g:ale_fix_on_save = 1

" 3.  Statusline integration – mimic the old SyntasticFlag indicator.

function! StatuslineALE() abort
  if exists('*ale#statusline#Count')
    let l:counts = ale#statusline#Count(bufnr(''))
    return l:counts.total ==# 0 ? '' : printf('[E:%d W:%d]', l:counts.error, l:counts.warning)
  endif
  return ''
endfunction

set statusline+=%#warningmsg#
set statusline+=%{StatuslineALE()}
set statusline+=%*


" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if line("'\"") >= 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

else

  set autoindent                " always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.

if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
                  \ | wincmd p | diffthis
endif

if has('langmap') && exists('+langnoremap')
  " Prevent that the langmap option applies to characters that result from a
  " mapping.  If unset (default), this may break plugins (but it's backward
  " compatible).
  set langnoremap
endif

" highlight trailing whitespace in red but not in Insert mode
" highlight whitespace when you open a new buffer
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()


" Remove trailing whitespace on save for *.py files
autocmd BufWritePre *.py :%s/\s\+$//e

" Toggle between paste and non-paste mode with <F2>
set pastetoggle=<F2>

if &term =~ '256color'
  " disable Background Color Erase (BCE) so that color schemes
  " render properly when inside 256-color term multiplexers
  set t_ut=
endif
