{ ... }:
{
  config.hosts = {
    python3.withPackages = p: [ p.pexpect ];
  };
}
