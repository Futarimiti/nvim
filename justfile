build *args='-L':
  nix build . {{args}}

[default]
run *args:
  nix run . {{args}}

runs *args:
  nix run . -- -S Session.vim {{args}}

# Update inputs && commit
update *inputs:
  #!/usr/bin/env python3
  def eprint(*args, **kwargs):
    import sys
    print(*args, file=sys.stderr, **kwargs)
  def run(cmdargs, check=True):
    import subprocess
    eprint(*cmdargs)
    return subprocess.run(cmdargs, check=check)
  inputs = '{{inputs}}'.split()
  run(['nix', 'flake', 'update', *inputs])
  if run(['git', 'diff', '--quiet', 'flake.lock'], check=False).returncode:
    run(['git', 'add', 'flake.lock'])
    inputs = ', '.join(inputs) or 'flake inputs'
    from datetime import datetime
    time = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    run(['git', 'commit', '-m', f'chore: update {inputs} on {time}'])
  else:
    eprint('No updates made')

cache: build
  cachix push futarimiti result
