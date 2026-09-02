#!/usr/bin/env bash
# Warn when the zotero-mcp semantic index falls behind the Zotero library.
set -uo pipefail

force=false
if [ "${1:-}" = --force ]; then
  force=true
fi

config="${ZOTERO_MCP_CONFIG:-$HOME/.config/zotero-mcp/config.json}"
api="${ZOTERO_LOCAL_API:-http://localhost:23119/api}"
# Runtime, not state. A reboot clears it and re-arms yesterday's warning.
flag="${XDG_RUNTIME_DIR:-/tmp}/zotero-index-check.stage"

# One "<library> <version>" line each. No key at all means the index was never built.
recorded="$(jq -r '
  .semantic_search as $s
  | ($s.last_sync_versions // (if $s.last_sync_version then {"0": $s.last_sync_version} else {} end))
  | to_entries[] | "\(.key) \(.value)"
' "$config" 2>/dev/null || true)"

# Ask Zotero for the current library version. Empty means it did not answer.
live_version() {
  local lib="$1" path
  if [ "$lib" = 0 ]; then path="users/0"; else path="groups/$lib"; fi
  curl -sf -m 5 -D - "$api/$path/items?limit=1&format=json" -o /dev/null 2>/dev/null \
    | tr -d '\r' \
    | sed -n 's/^[Ll]ast-[Mm]odified-[Vv]ersion:[[:space:]]*\([0-9][0-9]*\)$/\1/p' \
    | head -1
}

stage=fresh
summary=""

if ! curl -sf -m 5 "$api/users/0/items?limit=1&format=json" -o /dev/null 2>/dev/null; then
  # Zotero is closed or its API is off. Nothing to compare, so "cannot tell".
  stage=unreachable
elif [ -z "$recorded" ]; then
  stage=unbuilt
else
  behind=""
  while read -r lib recorded_version; do
    [ -n "$lib" ] || continue
    # Skip a library that does not answer. `|| true`: under errexit a 404 aborts here.
    current="$(live_version "$lib" || true)"
    [ -n "$current" ] || continue
    if [ "$current" != "$recorded_version" ]; then
      name="library $lib"
      [ "$lib" = 0 ] && name="My Library"
      behind="$behind${behind:+, }$name ($recorded_version -> $current)"
    fi
  done <<< "$recorded"

  if [ -n "$behind" ]; then
    stage=stale
    summary="$behind"
  fi
fi

# Always on stdout, so the journal records every verdict, not only the loud ones.
echo "stage: $stage${summary:+ ($summary)}"

prev="$(cat "$flag" 2>/dev/null || true)"
if [ "$force" = false ]; then
  # Nothing to do, and a closed Zotero is normal, not a problem.
  if [ "$stage" = fresh ] || [ "$stage" = unreachable ]; then
    printf '%s\n' "$stage" > "$flag"
    exit 0
  fi
  # One notification per transition. Adding papers all week must not warn per paper.
  if [ "$stage" = "$prev" ]; then
    exit 0
  fi
fi

case "$stage" in
  stale)
    title="Zotero semantic index is out of date"
    body="Behind: $summary
Run zotero-cli db update to catch it up."
    ;;
  unbuilt)
    title="Zotero semantic index has not been built"
    body="Semantic search will find nothing until it is.
Run zotero-cli db update to build it."
    ;;
  unreachable)
    # Only reachable under --force.
    title="Cannot tell whether the Zotero index is current"
    body="Zotero is not answering on its local API.
Start Zotero, or enable Settings -> Advanced -> allow other applications."
    ;;
  fresh)
    # Only reachable under --force.
    title="Zotero semantic index is current"
    body="Nothing to do."
    ;;
esac

# --expire-time=0 keeps it in the drawer. A message that fades is a message you miss.
notify-send --app-name=dotfiles --icon=accessories-dictionary --expire-time=0 \
  "$title" "$body"

# Write the flag only after the notification. A run before the daemon starts retries.
printf '%s\n' "$stage" > "$flag"
