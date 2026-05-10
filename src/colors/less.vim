" less is more

set background=dark

runtime! colors/default.vim
let g:colors_name = 'less'

" ui

highlight! CursorLine guibg=NvimDarkGrey3
highlight! Folded guifg=NvimDarkGrey4 guibg=NONE
highlight! NonText guibg=NONE
highlight! NonText guibg=NONE
highlight! Normal guibg=NONE
highlight! TabLine guibg=NONE guifg=Grey
highlight! TabLineSel guibg=NONE guifg=White gui=NONE
highlight! link ColorColumn CursorLine
highlight! link Conceal Special

" basics

highlight! Comment guifg=NvimLightGrey4 gui=italic
highlight! Delimiter guifg=NvimLightGrey2
highlight! Directory guifg=NvimLightGrey2 gui=bold
highlight! Function guifg=NvimLightGrey2
highlight! Identifier guifg=NvimLightGrey2
highlight! Module gui=italic guifg=NvimLightGrey2
highlight! Special guifg=NvimLightBlue
highlight! Type gui=bold
highlight! link Character String
highlight! link Constructor Type
highlight! link Method Function
highlight! link Namespace Module

" diagnostics

highlight! DiagnosticUnnecessary guifg=NvimLightGrey4
highlight! ErrorBG guibg=#4a302f gui=NONE
highlight! HintBG guibg=#3a5657 gui=NONE
highlight! WarnBG guibg=#4f3d2e gui=NONE
highlight! link DiagnosticUnderlineError ErrorBG
highlight! link DiagnosticUnderlineHint HintBG
highlight! link DiagnosticUnderlineWarn WarnBG

" treesitter

highlight! link @constant.builtin @constant
highlight! link @constructor Constructor
highlight! link @function.builtin @function
highlight! link @identifier Identifier
highlight! link @module Module
highlight! link @module.builtin Module
highlight! link @namespace Namespace
highlight! link @string.special.path Underlined
highlight! link @tag.attribute @attribute
highlight! link @type.builtin @type
highlight! link @variable.builtin @variable

" obsession

highlight! ObsessionActive gui=bold
highlight! ObsessionInactive gui=NONE

" noice

highlight! link NoicePopupmenuMatch Keyword
highlight! link NoiceSplit Normal

" per filetype
" haskell
highlight! link ConId Type
