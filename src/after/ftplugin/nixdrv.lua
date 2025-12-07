vim.wo.wrap = true

-- drvs are in ATerm format
-- (https://nix.dev/manual/nix/2.25/protocols/derivation-aterm)
-- no known ftplugins or treesitter parsers, rust syntax should be close enough
vim.treesitter.start(0, 'rust')
