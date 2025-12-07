if exists('b:did_indent')
  finish
endif

let b:did_indent = 1

setlocal indentexpr=haskell#get_indent()
setlocal indentkeys=!^F,o,O,=where,=deriving,==,0=in,0=class,0=instance,0=data,0=type,0=newtype,0=else,0<bar>,0},0],0(,0),0#,0,0==,0==>,0=->

let s:save_cpo = &cpo
set cpo&vim

let &cpo = s:save_cpo
unlet s:save_cpo
