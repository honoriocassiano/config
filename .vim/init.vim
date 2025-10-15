" ----------------------------------------------------------------------------------
" Plugin definitions
" ----------------------------------------------------------------------------------
set nocompatible              " be iMproved, required
filetype off                  " required

set rtp+=~/.vim/bundle/Vundle.vim,~/.vim/themes
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'preservim/nerdtree'
Plugin 'tomtom/tcomment_vim'
Plugin 'tpope/vim-dispatch'
Plugin 'tpope/vim-capslock'
Plugin 'godlygeek/tabular'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-sensible'
Plugin 'vimwiki/vimwiki'
Plugin 'tikhomirov/vim-glsl'
Plugin 'qpkorr/vim-bufkill'
Plugin 'Asheq/close-buffers.vim'
Plugin 'soramugi/auto-ctags.vim'
Plugin 'preservim/tagbar'
Plugin 'junegunn/goyo.vim'

call vundle#end()            " required
filetype plugin indent on    " required
syntax on

" :wshada sobrescreve o arquivo e remove os jumps salvos
" set shada=!,'1,<50,s10,h

autocmd VimEnter * exe 'tabdo windo clearjumps | tabnext'

" ----------------------------------------------------------------------------------
" VIM Editor settings
" ----------------------------------------------------------------------------------
" Config line numbers
set number
set relativenumber

" set mouse=a
set inccommand=nosplit

set autoindent
set autoread
set cursorline
set nowrap

set background=light

" Disable highlight search
set nohlsearch

" Config tabs
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4

" Open tabs on the right
set splitright

" Better undo history
set undofile

" Theme
" colorscheme default
colorscheme white

" Show useless whitespaces
let c_space_errors=1

set guifont=Iosevka\ Custom\ Semi-Extended:h13

set linespace=0

" Margem para rolagem vertical e horizontal
set scrolloff=3
set sidescroll=3

" Show trailing characters
set list
set listchars=tab:▶\ ,trail:¶

" Salva a porra (quase) toda na sessão
set sessionoptions=blank,buffers,curdir,folds,help,tabpages,winsize
fu! SessionSave()
    execute 'mksession! ' . getcwd() . '/.session.vim'
endfunction

fu! SessionRestore()
    let dir=getcwd()
    if filereadable(dir . '/.session.vim') && dir != $NVIM_HOME && dir != $HOME
        execute 'so ' . dir . '/.session.vim'
    endif
    if filereadable(dir . '/abbrev.vim') && dir != $NVIM_HOME
        execute 'so ' . dir . '/abbrev.vim'
    endif
endfunction

command -nargs=1 -complete=dir SessionCd call SessionCd(<q-args>)
function SessionCd(path)
    let seila=string(a:path)
    exe 'cd ' . a:path
    call SessionRestore()
    normal! :CtrlPDir :pwd<ESC>
endfunction

" Autocommands

augroup session
    au!
    au VimLeave * call SessionSave()
    au VimEnter * call SessionRestore()
augroup end

augroup alttab
    au!
    au FocusLost * silent! :wa
    au FocusGained,BufEnter [^\[]*[^\]] :checktime
augroup end

augroup cpp
    au!
    au FileType cpp setlocal ts=4 sts=4 sw=4
    au FileType c   setlocal ts=4 sts=4 sw=4

    au FileType cpp imap <F13> constexpr auto () noexcept -> void<cr>{<cr>}<esc>kkf(i
    au FileType cpp nmap <F13> iconstexpr auto () noexcept -> void<cr>{<cr>}<esc>kkf(i

    au FileType cpp imap <F12> =nullptr)<c-o>F=
    au FileType cpp nmap <F12> a=nullptr)<c-o>F=
augroup end

augroup web
    au!
    au FileType html setlocal ts=2 sts=2 sw=2
    au FileType css setlocal ts=2 sts=2 sw=2

    au BufNew,BufNewFile,BufRead *.ejs set filetype=html
augroup end

augroup premd
    au!
    au BufNew,BufNewFile,BufRead *.pmd set filetype=markdown
augroup end
" ----------------------------------------------------------------------------------
" Tabular settings
" ----------------------------------------------------------------------------------
" xnoremap t :Tabularize<space>/

" ----------------------------------------------------------------------------------
" Ctrlp settings
" ----------------------------------------------------------------------------------
let ctrlp_working_path_mode='ra'

set wildignore+=*/tmp/*,*/target/*,*.so,*.swp,*.zip            " MacOSX/Linux
set wildignore+=*\\tmp\\*,*\\target\\*,*.swp,*.zip,*.exe,*.dll " Windows

let g:ctrlp_follow_symlinks=1
let g:ctrlp_cmd='CtrlP :pwd'
let g:ctrlp_use_caching=0
let g:ctrlp_switch_buffer='ET'

let g:ctrlp_custom_ignore={
  \ 'dir':  '\v[\/](\.(git|hg|svn|target))|(build)|(node_modules)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ }
" ----------------------------------------------------------------------------------
" Tagbar settings
" ----------------------------------------------------------------------------------

imap <F8> <esc>:TagbarToggle<CR>
nmap <F8> :TagbarToggle<CR>

let g:tagbar_autofocus = 1
let g:tagbar_autoclose = 1
let g:tagbar_autoclose_netrw = 1

" ----------------------------------------------------------------------------------
" Tcommenter settings
" ----------------------------------------------------------------------------------
let g:tcomment_maps = 0
let g:tcomment#mode_extra = ''
let g:tcomment#blank_lines = 2

nnoremap <C-_> :TComment<cr>
vnoremap <C-_> :TComment<cr>
inoremap <C-_> <esc>:TComment<cr>i
nnoremap <C-S-_> :TComment<cr>
vnoremap <C-S-_> :TComment<cr>
inoremap <C-S-_> <esc>:TComment<cr>i

nnoremap <C-/> :TComment<cr>
vnoremap <C-/> :TComment<cr>
inoremap <C-/> <esc>:TComment<cr>i

nnoremap <C-d> mayyp<s-v>:TComment<cr>`a
inoremap <C-d> <esc>mayyp<s-v>:TComment<cr>`aa
vnoremap <C-d> may`]p`[v`]:TComment<cr>`a
" ----------------------------------------------------------------------------------
" NERDTree configurations
" ----------------------------------------------------------------------------------
nmap <F6> :NERDTreeToggle<CR>
imap <F6> <ESC>:NERDTreeToggle<CR>

let g:NERDTreeWinPos="right"
" Fecha a janela do NERDTree após abrir um arquivo
let NERDTreeQuitOnOpen=1

" ----------------------------------------------------------------------------------
" Dispatch
" ----------------------------------------------------------------------------------
let g:dispatch_no_maps = 1

nmap <F9> :wa <bar> Dispatch<cr>
imap <F9> <Esc>:wa <bar> Dispatch<cr>

" ----------------------------------------------------------------------------------
" Goyo
" ----------------------------------------------------------------------------------
autocmd! User GoyoEnter nested call <SID>ActivateWrap()
autocmd! User GoyoLeave nested call <SID>DeactivateWrap()

autocmd! VimLeavePre * :Goyo!


" ----------------------------------------------------------------------------------
" Shortcuts
" ----------------------------------------------------------------------------------
" Useful shortcuts
map <Space> <Leader>

nnoremap \ ^
vnoremap \ ^

nnoremap <Leader>p "+p
nnoremap <Leader>P "+P
vnoremap <Leader>p "+p
vnoremap <Leader>P "+P

nnoremap <Leader>y "+y
nnoremap <Leader>Y "+Y
vnoremap <Leader>y "+y
vnoremap <Leader>Y "+Y

vnoremap <Leader>q mu<Esc>`>a”<Esc>`<i“<Esc>`u

xnoremap p pgvy

nnoremap <Leader>s :vsplit ~/.vim/init.vim
" TODO Verificar se isso funciona no Windows
" nnoremap <Leader>s :exe "vsplit ".stdpath('config').'/init.vim'
" nnoremap <Leader>w :w<CR>

" nnoremap <Leader>i <C-i>
" nnoremap <Leader>o <C-o>

vmap > >gv
vmap < <gv

imap <A-k> <Up>
imap <A-j> <Down>
imap <A-h> <Left>
imap <A-l> <Right>
imap <A-w> <C-o>w
imap <A-W> <C-o>W
imap <A-e> <C-o>e
imap <A-E> <C-o>E
imap <A-b> <C-o>b
imap <A-B> <C-o>B

imap <Home> <c-o>^
nmap <Home> ^

inoremap <c-u> <c-g>u<c-u>
inoremap <c-w> <c-g>u<c-w>

" Duplicate lines in visual-line mode
" xnoremap <expr> <c-d> mode() ==# "V" ? "y`>p1vo" : "<c-d>"

:command W  w
:command Wq wq
:command WQ wq
:command Q  q

" nnoremap <Tab> <Esc>
" vnoremap <Tab> <Esc>gV
" onoremap <Tab> <Esc>
" inoremap <Tab> <Esc>`^
" inoremap <Leader><Tab> <Tab>

inoremap <C-CR> <Esc>O

" inoremap <C-d> <Esc>m`yyp``<Down>a
" nnoremap <C-d> m`yyp``<Down>

" Abrir o sublime em uma posição específica
command Subl call Subl()
function Subl()
    let file = expand('%')

    if empty(file)
        echohl ErrorMsg
        echom "No named buffer!"
        echohl None
    else
        let pos = getpos('.')
        let location = file . ':' . pos[1] . ':' . pos[2]

        let command = "!\"C:\\Program Files\\Sublime Text\\subl.exe\" -a " . location

        " Atualiza o arquivo e abre o Sublime
        :silent wa | silent exe command
    endif
endfunction

" Mostra as correspondências (buscas) no centro da tela
function! CenteredFindNext(forward)
    let s:so_curr=&scrolloff
    set scrolloff=999
    try
        if a:forward
            silent normal! n
        else
            silent normal! N
        endif
    finally
        let &scrolloff=s:so_curr
    endtry
endfunction

noremap <silent> <Leader>w :call ToggleWrap()<CR>
function ToggleWrap()
  if &wrap
    echo "Wrap OFF"
    setlocal nowrap
    set virtualedit=all
    silent! nunmap <buffer> <Up>
    silent! nunmap <buffer> <Down>
    silent! nunmap <buffer> <Home>
    silent! nunmap <buffer> <End>
    silent! iunmap <buffer> <Up>
    silent! iunmap <buffer> <Down>
    silent! iunmap <buffer> <Home>
    silent! iunmap <buffer> <End>
  else
    echo "Wrap ON"
    setlocal wrap linebreak nolist
    set virtualedit=
    setlocal display+=lastline
    noremap  <buffer> <silent> <Up>   gk
    noremap  <buffer> <silent> <Down> gj
    noremap  <buffer> <silent> <Home> g<Home>
    noremap  <buffer> <silent> <End>  g<End>
    inoremap <buffer> <silent> <Up>   <C-o>gk
    inoremap <buffer> <silent> <Down> <C-o>gj
    inoremap <buffer> <silent> <Home> <C-o>g<Home>
    inoremap <buffer> <silent> <End>  <C-o>g<End>
  endif
endfunction

function! s:ActivateWrap()
    if !&wrap
        call ToggleWrap()
    endif
endfunction

function! s:DeactivateWrap()
    if &wrap
        call ToggleWrap()
    endif
endfunction

nnoremap <silent>n :call CenteredFindNext(1)<CR>
nnoremap <silent>N :call CenteredFindNext(0)<CR>

nnoremap <F7>   :cn<CR>
nnoremap <S-F7> :cp<CR>

command -nargs=0 So execute "w | so %"

" ----------------------------------------------------------------------------------
" Busca
" ----------------------------------------------------------------------------------
:set wildignore+=*.o,*.obj,*.dll,*.exe,*.so

function! Ack(...)
    if a:0 < 2
        :exe 'noa vimgrep /'. a:1 . '/j ** | botright copen'
    else
        :exe 'noa vimgrep /'. a:1 . '/j ' . join(a:000[1:]) . ' | botright copen'
    endif
endfunction

let s:patterns = {
                \'cpp': 'h c cpp hpp',
                \'c': 'h c',
                \}

function! s:ExpandFilePattern(extensions)
    return join(map(split(a:extensions, ' '), {p, v -> '**/*.' . v}))
endfunction

function! AckType(pattern, filetype)
    :exe 'Ack ' . a:pattern . ' ' . s:ExpandFilePattern(s:patterns[a:filetype])
endfunction

function! AckThis(curr_word)
    if has_key(s:patterns, &filetype)
        return 'Ack ' . a:curr_word . ' ' . s:patterns[&filetype]
    else
        return 'Ack ' . a:curr_word
    endif
endfunction

nnoremap <F3> :<C-r>=AckThis('<C-r><C-w>')<cr><cr>

command -nargs=+ Ack call Ack(<f-args>)
command -nargs=+ Ackt call AckType(<f-args>)

command TODO execute "Ack TODO"

" ----------------------------------------------------------------------------------
" Copiar seleção visual para linha de comando
" ----------------------------------------------------------------------------------
" Post original: https://stackoverflow.com/questions/1533565/hot-to-get-visually-selected-text-in-vimscript#6271254
function! GetVisualSelection(sep)
    let [l_start, c_start] = getpos("'<")[1:2]
    let [l_end, c_end] = getpos("'>")[1:2]

    let lines = getline(l_start, l_end)
    if len(lines) == 0
        return ''
    endif

    let lines[-1] = lines[-1][: c_end - (&selection == 'inclusive' ? 1 : 2)]
    let lines[0] = lines[0][c_start -1:]

    return join(lines, a:sep)
endfunction

cnoremap <C-r><C-v> <C-r>=GetVisualSelection(' ')<cr>

" Grep
if executable("rg")
    " set grepprg=rg\ --vimgrep\ --smart-case\ --hidden
    " set grepformat=%f:%l:%c:%m

    let g:ctrlp_user_command = 'rg -g "!{target,build,node_modules}"  %s --files --color=never --glob ""'
    let g:ctrlp_use_caching = 0
endif

" ----------------------------------------------------------------------------------
" Neovide
" ----------------------------------------------------------------------------------
let g:neovide_refresh_rate = 165
let g:neovide_refresh_rate_idle = 165

" ----------------------------------------------------------------------------------
" Statusline
" ----------------------------------------------------------------------------------
let g:currentmode={
       \ 'n'  : 'NORMAL',
       \ 'v'  : 'VISUAL',
       \ 'V'  : 'V·Line',
       \ "\<C-V>" : 'V·Block',
       \ 'i'  : 'INSERT',
       \ 'R'  : 'R',
       \ 'Rv' : 'V·Replace',
       \ 'c'  : 'Command',
       \ 't'  : 'Terminal',
       \}

function CassWrapModeName()
    if exists('b:cass_wrap_mode') && b:cass_wrap_mode
        return ' [WRAP ON]'
    else
        return ''
    endif
endfunction
set statusline=
set statusline+=\ [%{toupper(g:currentmode[mode()])}]
set statusline+=%{CassWrapModeName()}
set statusline+=\ (%l,\ %c)
set statusline+=\ %f%m\ %y
set statusline+=\ [%L\ lines]

" ----------------------------------------------------------------------------------
" Tabs
" ----------------------------------------------------------------------------------
" Navegar as abas com Alt+{num}

nnoremap <A-1> 1gt
nnoremap <A-2> 2gt
nnoremap <A-2> 2gt
nnoremap <A-3> 3gt
nnoremap <A-4> 4gt
nnoremap <A-5> 5gt
nnoremap <A-6> 6gt
nnoremap <A-7> 7gt
nnoremap <A-6> 6gt

inoremap <A-1> <esc>1gt
inoremap <A-2> <esc>2gt
inoremap <A-2> <esc>2gt
inoremap <A-3> <esc>3gt
inoremap <A-4> <esc>4gt
inoremap <A-5> <esc>5gt
inoremap <A-6> <esc>6gt
inoremap <A-7> <esc>7gt
inoremap <A-6> <esc>6gt

vnoremap <A-1> 1gt
vnoremap <A-2> 2gt
vnoremap <A-2> 2gt
vnoremap <A-3> 3gt
vnoremap <A-4> 4gt
vnoremap <A-5> 5gt
vnoremap <A-6> 6gt
vnoremap <A-7> 7gt
vnoremap <A-6> 6gt

" No continuation comment on new line
autocmd BufNewFile,BufRead * setlocal formatoptions-=cro
autocmd BufNew,BufNewFile,BufRead * let b:cass_wrap_mode=0

augroup vim
    au!
    au FileType vim inoremap <F5> <esc>:So<cr>
    au FileType vim nnoremap <F5> :So<cr>
augroup end

nmap <F1> q:
imap <F1> <nop>
nmap <F4> :q<cr>
imap <F4> <Esc>:q<cr>

nmap <Esc> :

" cnoremap s/ %smagic/

nnoremap <F2> :silent !explorer.exe .<cr>
" match TrailingSpaceChar /\s\+$/
" match TabChar /\t\+/
"
" autocmd BufWinEnter * match TrailingSpaceChar /\s+$/
" autocmd InsertEnter * match TrailingSpaceChar /\s+$/
" autocmd InsertLeave * match TrailingSpaceChar /\s+$/
" autocmd BufWinEnter * match TabChar /\t\+/
" autocmd InsertEnter * match TabChar /\t\+/
" autocmd InsertLeave * match TabChar /\t\+/
" autocmd BufWinLeave * call clearmatches()

" ----------------------------------------------------------------------------------
" Apagar coisas como no sublime
" ----------------------------------------------------------------------------------
inoremap <C-Backspace> <C-w>
cnoremap <C-Backspace> <C-w>

inoremap <C-Delete> <C-o>de
nnoremap <C-Delete> de

cnoremap <S-Delete> <C-u>
inoremap <S-Delete> <C-o>dd
nnoremap <S-Delete> dd
" cmap <C-Delete> <???> " TODO Encontrar o comando para apagar uma palavra para a frente

" cnoremap <C-r><C-v> ??? " TODO Implementar cópia do modo visual para a janela de comandos

" ----------------------------------------------------------------------------------
" Folds
" ----------------------------------------------------------------------------------
set foldmethod=indent
set foldnestmax=10
set nofoldenable
set foldlevel=2

" set virtualedit=all
" set signcolumn=yes

" ----------------------------------------------------------------------------------
" User Command mode
" ----------------------------------------------------------------------------------
command CommandMode call UserCommands()
function! UserCommands()
    if bufexists('<COMMANDS>')==1
        :let l:bufid = bufnr('<COMMANDS>')
        :exe "buf " . l:bufid
    else
        if bufname() != ""
            :enew|set bt=nofile ft=user_commands
        else
            :set bt=nofile ft=user_commands
        endif
        :let g:usr_commands_buf_id = bufnr()
        :file <COMMANDS>
    endif
endfunction

function! UserExecuteCommand()
    :mark u
    :exe "r!" . getline('.')
    :'u
endfunction

function! UserExecuteVimCommand()
    :mark u
    :exe "pu = " . "trim(execute(\'" . getline('.') . "\'))"
    :'u
endfunction

augroup usr_cmds
    au!
    au FileType user_commands nmap <F12> <cmd>call UserExecuteCommand()<cr>
    au FileType user_commands nmap <F13> <cmd>call UserExecuteVimCommand()<cr>
augroup end

" ----------------------------------------------------------------------------------
" Terminal embutido
" ----------------------------------------------------------------------------------
command -nargs=? Term tabe|term <f-args>

tnoremap <C-\><C-\> <C-\><C-n>

" ----------------------------------------------------------------------------------
" Título
" ----------------------------------------------------------------------------------
set title

" ----------------------------------------------------------------------------------
" Aumentar e diminuir fonte
" ----------------------------------------------------------------------------------
let s:font_size = 12
let s:font_name = 'Iosevka\ Custom\ Semi-Extended'

exe 'set guifont=' . s:font_name . ':h' . s:font_size 
" set guifont=Fira\ Mono:h11
" set guifont=Fira\ Code:h10
" set guifont=Monolisa:h9

function! AdjustFontSize(amount)
  let s:font_size = s:font_size+a:amount
  :execute "GuiFont! " . s:font_name . ":h" . s:font_size
endfunction

noremap <C-ScrollWheelUp> :call AdjustFontSize(1)<CR>
noremap <C-ScrollWheelDown> :call AdjustFontSize(-1)<CR>
inoremap <C-ScrollWheelUp> <Esc>:call AdjustFontSize(1)<CR>a
inoremap <C-ScrollWheelDown> <Esc>:call AdjustFontSize(-1)<CR>a

noremap <c-PageUp> :call AdjustFontSize(1)<CR>
noremap <c-PageDown> :call AdjustFontSize(-1)<CR>
inoremap <C-PageUo> <Esc>:call AdjustFontSize(1)<CR>a
inoremap <C-PageDown> <Esc>:call AdjustFontSize(-1)<CR>a

" ----------------------------------------------------------------------------------
" Lista de jumps
" ----------------------------------------------------------------------------------
function! GotoJump()
  jumps
  let j = input("Please select your jump: ")
  if j != ''
    let pattern = '\v\c^\+'
    if j =~ pattern
      let j = substitute(j, pattern, '', 'g')
      execute "normal " . j . "\<c-i>"
    else
      execute "normal " . j . "\<c-o>"
    endif
  endif
endfunction
nnoremap <Leader>j :call GotoJump()<CR>

set clipboard+=unnamedplus

" ----------------------------------------------------------------------------------
" Não-funções uteis
" ----------------------------------------------------------------------------------
" TODO não funciona por algum motivo
function! DontUse_PrintHighlight()
    :echo 'echo synIDattr(synID(line("."), col("."), 1), "name")'
endfunction

" Abrir o resultado de comando
function! DontUse_BufferWithCommandResult()
    :echo 'enew|pu=execute("?")|set bt=nofile'
endfunction
