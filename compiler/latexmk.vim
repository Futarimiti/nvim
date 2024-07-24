if exists("current_compiler")
  finish
endif
let current_compiler = "latexmk"

let s:cpo_save = &cpo
set cpo&vim

CompilerSet errorformat=%E!\ LaTeX\ %trror:\ %m,
      \%E!\ %m,
      \%+WLaTeX\ %.%#Warning:\ %.%#line\ %l%.%#,
      \%+W%.%#\ at\ lines\ %l--%*\\d,
      \%WLaTeX\ %.%#Warning:\ %m,
      \%Cl.%l\ %m,
      \%+C\ \ %m.,
      \%+C%.%#-%.%#,
      \%+C%.%#[]%.%#,
      \%+C[]%.%#,
      \%+C%.%#%[{}\\]%.%#,
      \%+C<%.%#>%.%#,
      \%C\ \ %m,
      \%-GSee\ the\ LaTeX%m,
      \%-GType\ \ H\ <return>%m,
      \%-G\ ...%.%#,
      \%-G%.%#\ (C)\ %.%#,
      \%-G(see\ the\ transcript%.%#),
      \%-G\\s%#,
      \%+O(%*[^()])%r,
      \%+O%*[^()](%*[^()])%r,
      \%+P(%f%r,
      \%+P\ %\\=(%f%r,
      \%+P%*[^()](%f%r,
      \%+P[%\\d%[^()]%#(%f%r,
      \%+Q)%r,
      \%+Q%*[^()])%r,
      \%+Q[%\\d%*[^()])%r
CompilerSet makeprg=latexmk

let &cpo = s:cpo_save
unlet s:cpo_save
