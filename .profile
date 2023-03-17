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

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin::$PATH"
fi

export XDG_DATA_HOME="$HOME"/.local/share
export CABAL_DIR="$XDG_DATA_HOME"/cabal

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
