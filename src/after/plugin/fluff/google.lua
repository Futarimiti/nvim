-- just google it

local urls = {
  Google = 'https://www.google.com/search?q=%s',
  DuckDuckGo = 'https://duckduckgo.com/?q=%s',
  Reddit = 'https://www.reddit.com/r/neovim/search/?q=%s',
  Nixpkgs = 'https://search.nixos.org/packages?channel=unstable&query=%s',
  Hoogle = 'https://hoogle.haskell.org/?hoogle=%s',
  Stackage = 'https://www.stackage.org/nightly-2025-04-22/hoogle?q=%s',
  Noogle = 'https://noogle.dev/q?term=%s',
}

vim.iter(urls):each(function(engine, url)
  vim.api.nvim_create_user_command(
    engine,
    function(o) vim.ui.open(url:format(vim.uri_encode(o.args))) end,
    { nargs = '?', desc = ('search on %s'):format(engine) }
  )
end)
