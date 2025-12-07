" setlocal textwidth=80
let &l:formatprg = $'typstyle --line-width {&l:textwidth ?? 80} --wrap-text --quiet'
setlocal iskeyword+=:,-,#
