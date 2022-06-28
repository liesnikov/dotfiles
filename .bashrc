# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# Eternal bash history. https://stackoverflow.com/a/19533853
# Undocumented feature which sets the size to "unlimited".
# http://stackoverflow.com/questions/9457233/unlimited-bash-history
export HISTFILESIZE=-1
export HISTSIZE=-1
export HISTTIMEFORMAT="[%F %T] "
# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history

# After each command, append to the history file and reread it
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
  color_prompt=yes
    else
  color_prompt=
    fi
fi


# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

if [ -f "$HOME"/.bash_aliases ]; then
  . "$HOME"/.bash_aliases
fi

if [ -f "$HOME"/.bash_functions ]; then
  . "$HOME"/.bash_functions
fi

if direnv --version &> /dev/null; then
  eval "$(direnv hook bash)"
fi

export PS1='\[\e[1m\]\W\[\e[0m\] ⊢ '

# virtualenv for python wrapper
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
export WORKON_HOME=/home/buzzer/.virtualenvs
source ~/.local/bin/virtualenvwrapper_lazy.sh
export PIP_VIRTUALENV_BASE=/home/buzzer/.virtualenvs

# set vi mode for navigating and stuff
# set -o vi

#so as not to be disturbed by Ctrl-S ctrl-Q in terminals:
stty -ixon

export LOCALE_ARCHIVE="/usr/lib/locale/locale-archive"


# If not running interactively, do not do anything otherwise start tmux
[[ $- != *i* ]] && return
[[ -z "$TMUX" ]] && [[ "$TERM" != "dumb" ]] && exec tmux
