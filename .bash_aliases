alias mclip='xclip -selection clipboard'
alias tmux="tmux -2"
alias lessfollow="less --follow-name +F"
# alias ycm-generate="~/.vim/bundle/YCM-Generator/config_gen.py"

#alias caps-unlock="python3 -c 'from ctypes import *; X11 = cdll.LoadLibrary(\"libX11.so.6\"); display = X11.XOpenDisplay(None); X11.XkbLockModifiers(display, c_uint(0x0100), c_uint(2), c_uint(0)); X11.XCloseDisplay(display)'"
#alias CAPS-UNLOCK="caps-unlock"
alias keyboard-set="setxkbmap -option ctrl:nocaps ; xset r rate 200 70"
alias KEYBOARD-SET="keyboard-set"

alias untar-gz="tar -xvzf"
alias emacso="emacsclient -n -c -a \"\""
alias i3lock="i3lock -c 000000"
alias lock="dm-tool lock"
alias suspend="systemctl suspend"
