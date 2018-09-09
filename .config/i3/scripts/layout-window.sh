#!/bin/bash
i3subscribe window | grep window:focus | \
while read -r line; do
    id="$(xprop -root _NET_ACTIVE_WINDOW | awk '{print $5}')"
    case "$(xprop -id "$id" WM_CLASS | cut -d\" -f4)" in
        Firefox)        kb=de ;;
        ChatProgram)    kb=ru ;;
        *)              kb=us ;;
    esac
    setxkbmap "$kb"
done
