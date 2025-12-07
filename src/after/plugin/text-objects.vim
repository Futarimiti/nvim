onoremap aa a<
onoremap ia i<
onoremap ar a[
onoremap ir i[
xnoremap aa a<
xnoremap ia i<
xnoremap ar a[
xnoremap ir i[

" vim-textobj

packadd vim-textobj-user
packadd vim-textobj-entire   " ae, ie

let g:textobj_comment_no_default_key_mappings = 1
packadd vim-textobj-comment
xmap a/ <Plug>(textobj-comment-a)
omap a/ <Plug>(textobj-comment-a)
xmap i/ <Plug>(textobj-comment-i)
omap i/ <Plug>(textobj-comment-i)
xmap a* a/
omap a* a/
xmap i* i/
omap i* i/

" TS textobjs defined in after/plugin/treesitter.vim
