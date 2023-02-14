# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
      . "$HOME/.bashrc"
    fi
fi

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

export ICEAUTHORITY="$XDG_CACHE_HOME"/ICEauthority

export OPAMROOT="$XDG_DATA_HOME/opam"

# $HOME/.nvidia-settings-rc
# nvidia-settings --config="$XDG_CONFIG_HOME"/nvidia/settings

export UNISON="$XDG_DATA_HOME"/unison

export VAGRANT_HOME="$XDG_DATA_HOME"/vagrant

alias wget=wget --hsts-file="$XDG_DATA_HOME/wget-hsts"

export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority

# xrdb -load "$XDG_CONFIG_HOME/X11/xresources"

ERRFILE="$XDG_CACHE_HOME/X11/xsession-errors"

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

test -r $OPAMROOT/opam-init/init.sh && . $OPAMROOT/opam-init/init.sh > /dev/null 2> /dev/null || true  # by opam, ml package manager

if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer


# for KeePassXC, since it doesn't respect system's theme on ubuntu 18.04
# https://github.com/keepassxreboot/keepassxc/issues/1931
# works with qt5-style-plugins
export QT_QPA_PLATFORMTHEME=gtk2
#export QT_AUTO_SCREEN_SCALE_FACTOR=0
export GHCUP_USE_XDG_DIRS=1
