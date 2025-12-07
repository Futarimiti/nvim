" Quick fuzzy find in directories

cnoremap <C-Space> <NOP>

" file
nnoremap <LocalLeader>f :edit **/
nnoremap <LocalLeader>g :vimgrep  **<Left><Left><Left>
cnoremap <C-Space>f **/
cmap <C-Space><C-F> <C-Space>f

" shipped runtime
nnoremap <LocalLeader>r :edit $VIMRUNTIME/**/
nnoremap <LocalLeader>R :vimgrep  $VIMRUNTIME/**<C-Left><Left>
cnoremap <nowait> <C-Space>r $VIMRUNTIME/**/
cmap <nowait> <C-Space><C-R> <C-Space>r

" scriptnames
nnoremap <LocalLeader>s :scriptnames **/
nnoremap <LocalLeader>S
      \ :vimgrep  `=getscriptinfo()->map({_,f->f.name})`<C-Left><Left>

lua << EOF
-- nix
local nix_darwin = vim.fs.normalize '$XDG_CONFIG_HOME/nix-darwin'
if vim.fn.isdirectory(nix_darwin) == 0 then return end
nix_darwin = vim.fn.fnamemodify(nix_darwin, ':~')
vim.keymap.set('n', '<LocalLeader>n', ':edit ' .. nix_darwin .. '/**/')
vim.keymap.set('n', '<LocalLeader>N', ':vimgrep  ' .. nix_darwin .. '/**<C-Left><Left>')
vim.keymap.set('c', '<C-Space>n', nix_darwin .. '/**/', { nowait = true })
vim.keymap.set('c', '<C-Space><C-N>', '<C-Space>n', { nowait = true, remap = true })

-- oldfiles
vim.api.nvim_create_user_command('BrowseOldfiles', function(o) vim.cmd.edit(o.args) end, {
  desc = 'fuzzy search in oldfiles',
  nargs = 1,
  complete = function(arg, _, _)
    return vim
      .iter(vim.v.oldfiles)
      :filter(function(f) return vim.re.find(f, vim.glob.to_lpeg(arg .. '*')) ~= nil end)
      :totable()
  end,
})
vim.keymap.set('n', '<LocalLeader>o', ':BrowseOldfiles ')
vim.keymap.set('n', '<LocalLeader>O', ':vimgrep  `=v:oldfiles`<C-Left><Left>')
EOF
