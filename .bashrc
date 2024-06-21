# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# setting XDG variables
export XDG_DATA_HOME="$HOME"/.local/share
export XDG_CONFIG_HOME="$HOME"/.config
export XDG_STATE_HOME="$HOME"/.local/state
export XDG_CACHE_HOME="$HOME"/.cache

# a bunch of bullshit variables to move them to xdg
export ANDROID_HOME="$XDG_DATA_HOME"/android

export CABAL_CONFIG="$XDG_CONFIG_HOME"/cabal/config
export CABAL_DIR="$XDG_DATA_HOME"/cabal

export CARGO_HOME="$XDG_DATA_HOME"/cargo

export CUDA_CACHE_PATH="$XDG_CACHE_HOME"/nv

export GEM_HOME="${XDG_DATA_HOME}"/gem
export GEM_SPEC_CACHE="${XDG_CACHE_HOME}"/gem

export GNUPGHOME="$XDG_DATA_HOME"/gnupg

export IPYTHONDIR="${XDG_CONFIG_HOME}/ipython"

export JULIA_DEPOT_PATH="$XDG_DATA_HOME/julia:$JULIA_DEPOT_PATH"

export JUPYTER_CONFIG_DIR="$XDG_CONFIG_HOME"/jupyter

export LESSHISTFILE="$XDG_STATE_HOME"/less/history

# export ICEAUTHORITY="$XDG_CACHE_HOME"/ICEauthority

export OPAMROOT="$XDG_DATA_HOME/opam"

# $HOME/.nvidia-settings-rc
# nvidia-settings --config="$XDG_CONFIG_HOME"/nvidia/settings

export UNISON="$XDG_DATA_HOME"/unison

export VAGRANT_HOME="$XDG_DATA_HOME"/vagrant

alias wget=wget --hsts-file="$XDG_DATA_HOME/wget-hsts"

# export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority

# xrdb -load "$XDG_CONFIG_HOME/X11/xresources"

# ERRFILE="$XDG_CACHE_HOME/X11/xsession-errors"

# set ssh to be authentificated via 1password
export SSH_AUTH_SOCK=~/.1password/agent.sock

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin::$PATH"
fi

if [ -d "$CABAL_DIR/bin" ] ; then
    PATH="$CABAL_DIR/bin:$PATH"
fi

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# Eternal bash history. https://stackoverflow.com/a/19533853
# Undocumented feature which sets the size to "unlimited".
# http://stackoverflow.com/questions/9457233/unlimited-bash-history
export HISTFILESIZE=
export HISTSIZE=
export HISTTIMEFORMAT="[%F %T] "
# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE="${XDG_DATA_HOME}"/.bash_eternal_history

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

# set vi mode for navigating and stuff
# set -o vi

#so as not to be disturbed by Ctrl-S ctrl-Q in terminals:
stty -ixon

export LOCALE_ARCHIVE="/usr/lib/locale/locale-archive"


# If not running interactively, do not do anything otherwise start tmux
[[ $- != *i* ]] && return
[[ -z "$TMUX" ]] && [[ "$TERM" != "dumb" ]] && exec tmux
