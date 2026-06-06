" setlocal shiftwidth=2
" setlocal tabstop=2
" setlocal expandtab
setlocal iskeyword+=-
setlocal suffixesadd+=/default.nix

setlocal textwidth=80
let &l:formatprg = $'nixfmt --width={&l:textwidth} -'
