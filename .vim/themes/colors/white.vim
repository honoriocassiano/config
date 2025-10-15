" Vim color file
" white
" Created by  with ThemeCreator (https://github.com/mswift42/themecreator)

hi clear

if exists("syntax on")
syntax reset
endif

set t_Co=256
let g:colors_name="white"

let s:basew="#ffffff"
" let s:basew="#fbfbfb"
" let s:basew="#d4d4d4"
let s:baseb="#000000"
" let s:baseb="#63788b"
" let s:baseb="#424949"
let s:baseg="#c3c3c3"
" let s:baser="#c678dd"
" let s:baser="#B7D7E8"
" Verde maneiro
" let s:baser="#B1CBBB"
" Vermelho original
let s:baser="#e74c3c"

" Define reusable colorvariables.
let s:bg=s:basew
let s:fg=s:baseb
let s:bg2=s:baseg
let s:keyword=s:fg
let s:builtin=s:fg
let s:const= s:fg
let s:comment=s:fg
let s:func=s:fg
let s:str=s:fg
let s:type=s:fg
let s:var=s:fg
let s:wa=s:baser
let s:warning=s:wa
let s:warning2=s:wa

exe 'hi Normal guifg='s:fg' guibg='s:bg
exe 'hi Visual guifg='s:bg2' guibg='s:fg
exe 'hi Cursor guifg='s:bg' guibg='s:fg
exe 'hi CursorLine  guibg='s:bg' gui=underline'
exe 'hi CursorColumn  guibg='s:bg
exe 'hi ColorColumn  guibg='s:bg
exe 'hi FoldColumn guifg='s:comment' guibg='s:bg
exe 'hi SignColumn guifg='s:comment' guibg='s:bg
" exe 'hi LineNr guifg='s:fg' guibg='s:fg2
exe 'hi CursorLineNr guifg='s:fg' guibg='s:bg' gui=underline,bold'
exe 'hi VertSplit guifg='s:fg' guibg='s:bg
exe 'hi MatchParen guifg='s:warning2'  gui=underline'
exe 'hi StatusLine guifg='s:bg' guibg='s:wa' gui=bold'
exe 'hi Pmenu guifg='s:fg' guibg='s:bg
exe 'hi PmenuSel  guibg='s:bg
exe 'hi IncSearch guifg='s:bg' guibg='s:keyword
exe 'hi Search   gui=underline'
exe 'hi Directory guifg='s:const
exe 'hi Folded guifg='s:fg' guibg='s:bg
exe 'hi WildMenu guifg='s:str' guibg='s:bg

exe 'hi Boolean guifg='s:const
exe 'hi Character guifg='s:const
exe 'hi Comment guifg='s:comment' gui=italic'
exe 'hi Conditional guifg='s:keyword' gui=bold'
exe 'hi Constant guifg='s:const
exe 'hi Todo guibg='s:bg
exe 'hi Define guifg='s:keyword
exe 'hi DiffAdd guifg=#000000 guibg=#bef6dc gui=bold'
exe 'hi DiffDelete guifg='s:bg
exe 'hi DiffChange  guibg=#5b76ef guifg=#ffffff'
exe 'hi DiffText guifg=#ffffff guibg=#ff0000 gui=bold'
exe 'hi TabChar guifg='s:warning' guibg='s:bg' gui=bold'
exe 'hi ErrorMsg guifg='s:warning' guibg='s:bg' gui=bold'
exe 'hi WarningMsg guifg='s:fg' guibg='s:warning2
exe 'hi Float guifg='s:const
exe 'hi Function guifg='s:func
exe 'hi Identifier guifg='s:type
exe 'hi Keyword guifg='s:keyword'  gui=bold'
exe 'hi Label guifg='s:var' gui=bold'
exe 'hi NonText guifg='s:bg' guibg='s:bg
exe 'hi Number guifg='s:const
exe 'hi Operator guifg='s:keyword
exe 'hi PreProc guifg='s:keyword
exe 'hi Special guifg='s:fg
exe 'hi SpecialKey guifg='s:fg' guibg='s:warning
exe 'hi Statement guifg='s:keyword
exe 'hi StorageClass guifg='s:type'  gui=italic'
exe 'hi String guifg='s:str
exe 'hi Tag guifg='s:keyword
exe 'hi Title guifg='s:fg'  gui=bold'
exe 'hi Todo guifg='s:bg' guibg='s:fg'  gui=bold'
exe 'hi Type guifg='s:type
exe 'hi Underlined   gui=underline'

exe 'hi TabChar guifg='s:fg' guibg='s:warning
exe 'hi TrailingSpaceChar guifg='s:fg' guibg='s:wa

" C/C++ Highlight
exe 'hi cInclude guifg='s:keyword' gui=NONE'
exe 'hi cIncluded guifg='s:keyword' gui=NONE'
exe 'hi cType guifg='s:keyword
exe 'hi cCppOutWrapper guifg='s:keyword' gui=NONE'
exe 'hi cStorageClass guifg='s:keyword' gui=bold'
exe 'hi cppStorageClass guifg='s:keyword' gui=bold'

" Python Highlighting
exe 'hi pythonBuiltinFunc guifg='s:builtin

" Javascript Highlighting
exe 'hi jsBuiltins guifg='s:builtin
exe 'hi jsFunction guifg='s:keyword' gui=bold'
exe 'hi jsGlobalObjects guifg='s:type
exe 'hi jsAssignmentExps guifg='s:var

" Html Highlighting
exe 'hi htmlLink guifg='s:var' gui=underline'
exe 'hi htmlStatement guifg='s:keyword
exe 'hi htmlSpecialTagName guifg='s:keyword

" Markdown Highlighting
exe 'hi mkdCode guifg='s:builtin

exe 'hi TagBarSignature guifg='s:fg' guibg='s:bg


exe 'hi PmenuSel guifg='s:bg' guibg='s:fg

exe 'hi @variable guifg='s:fg' guibg='s:bg ' gui=NONE'
" exe 'hi link @variable Normal'
