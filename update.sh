#!/usr/bin/env bash

source "$DOTFILES/colors.sh"
source "$DOTFILES/install_functions.sh"
# source "$DOTFILES/install_config"

#---------------------------------------

# backup zsh history file
# mkdir -p "$DOTFILES_CLOUD/zsh"
cp "$HISTFILE" "$HISTFILE-$(date +%F)"

#---------------------------------------

dot_mes_warn "Activate sudo"
sudo echo "Sudo activated!"

#---------------------------------------

dot_mes_update "Neovim plugins"
nvim --noplugin +PlugUpdate +qa

#---------------------------------------

sudo pacman -Syu

if hash yay 2>/dev/null; then
    yay -Syu
fi
