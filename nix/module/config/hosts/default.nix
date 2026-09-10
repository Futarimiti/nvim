_:
{
  config.hosts = {
    python3.withPackages = p: [ p.pexpect ];
    ruby.nvim-host.enable = false;
    perl.nvim-host.enable = false;
    node.nvim-host.enable = false;
  };
}
