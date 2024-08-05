set background=dark

runtime! colors/default.vim
let g:colors_name = 'less'

" primitives

highlight! Function guifg=NvimLightGrey2
highlight! Identifier guifg=NvimLightGrey2
highlight! Delimiter guifg=NvimLightGrey2
highlight! Directory guifg=NvimLightGrey2 cterm=bold gui=bold
highlight! Module cterm=italic gui=italic
highlight! Special guifg=NvimLightBlue
highlight! link Namespace Module
highlight! Type cterm=bold gui=bold
highlight! link Constructor Type
highlight! link Character String

" treesitter

highlight! link @namespace Namespace
highlight! link @module Module
highlight! link @string.special.path Underlined
highlight! link @constructor Constructor

highlight! link @type.builtin @type
highlight! link @variable.builtin @variable
highlight! link @constant.builtin @constant
highlight! link @function.builtin @function
