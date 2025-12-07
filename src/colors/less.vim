" less is more

set background=dark

runtime! colors/default.vim
let g:colors_name = 'less'

" ui

highlight! CursorLine guibg=NvimDarkGrey3
highlight! link ColorColumn CursorLine
highlight! TabLine guibg=NONE guifg=Grey
highlight! TabLineSel guibg=NONE guifg=White gui=NONE
highlight! NonText guibg=NONE
highlight! Normal guibg=NONE
highlight! NonText guibg=NONE
highlight! Folded guifg=NvimDarkGrey4 guibg=NONE

" basics

highlight! Function guifg=NvimLightGrey2
highlight! link Method Function
highlight! Identifier guifg=NvimLightGrey2
highlight! Delimiter guifg=NvimLightGrey2
highlight! Directory guifg=NvimLightGrey2 gui=bold
highlight! Module gui=italic guifg=NvimLightGrey2
highlight! Special guifg=NvimLightBlue
highlight! link Namespace Module
highlight! Type gui=bold
highlight! link Constructor Type
highlight! link Character String
highlight! Comment guifg=NvimLightGrey4 gui=italic

" diagnostics
highlight! ErrorBG guibg=#4a302f gui=NONE
highlight! WarnBG guibg=#4f3d2e gui=NONE
highlight! HintBG guibg=#3a5657 gui=NONE
highlight! link DiagnosticUnderlineError ErrorBG
highlight! link DiagnosticUnderlineWarn WarnBG
highlight! link DiagnosticUnderlineHint HintBG
highlight! DiagnosticUnnecessary guifg=NvimLightGrey4

" treesitter

highlight! link @namespace Namespace
highlight! link @module Module
highlight! link @module.builtin Module
highlight! link @string.special.path Underlined
highlight! link @constructor Constructor
highlight! link @identifier Identifier
" not sure the purpose of underscore-prefix
" highlight! link @_method Method
" highlight! link @_op Operator
" highlight! link @_name @identifier

highlight! link @type.builtin @type
highlight! link @variable.builtin @variable
highlight! link @constant.builtin @constant
highlight! link @function.builtin @function
highlight! link @tag.attribute @attribute

" obsession

highlight! ObsessionActive gui=bold
highlight! ObsessionInactive gui=NONE

" noice

highlight! link NoicePopupmenuMatch Keyword
highlight! link NoiceSplit Normal

" per filetype
highlight! link ConId Type
