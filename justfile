# Update inputs && commit
update *inputs:
  git checkout flake
  nix flake update {{inputs}}
  @if git diff --quiet flake.lock; \
  then echo "No updates made"; \
  else \
    git add flake.lock; \
    git commit -m "update(flake.lock): $(date '+%Y-%m-%d %H:%M:%S')"; \
  fi
