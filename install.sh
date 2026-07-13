#!/usr/bin/env bash
# Symlink tracked dotfiles into $HOME. Idempotent; safe to re-run.
#
#   ./install.sh        apply
#   ./install.sh -n     dry-run (show what would change, touch nothing)
#
# Every tracked file is symlinked to its matching path under $HOME, EXCEPT
# files marked `deploy=no` in .gitattributes (repo tooling, git-config
# fragments included by path, machine-churned files, ...).
#
# Files already reachable through an existing symlink (including a symlinked
# PARENT directory, e.g. ~/.config/emacs -> repo) are left untouched. Existing
# real (non-symlink) files are backed up to <path>.prelink.bak before replacing.
set -euo pipefail
repo="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
dry=0; [ "${1:-}" = "-n" ] && dry=1
cd "$repo"

git ls-files -z | while IFS= read -r -d '' f; do
    [ "$(git check-attr deploy -- "$f" | sed 's/^.*: deploy: //')" = no ] && continue
    src="$repo/$f"; dst="$HOME/$f"
    # already deployed (direct symlink, or reachable via a symlinked parent dir)?
    [ -e "$dst" ] && [ "$(readlink -f "$dst")" = "$(readlink -f "$src")" ] && continue
    if [ "$dry" = 1 ]; then
        if   [ -L "$dst" ]; then echo "RELINK             ~/$f"
        elif [ -e "$dst" ]; then echo "REPLACE(real file) ~/$f"
        else                     echo "LINK               ~/$f"; fi
        continue
    fi
    mkdir -p "$(dirname "$dst")"
    [ -e "$dst" ] && [ ! -L "$dst" ] && cp -a "$dst" "$dst.prelink.bak"
    if ln -sfn "$src" "$dst"; then echo "linked ~/$f"; else echo "WARN: could not link ~/$f" >&2; fi
done
