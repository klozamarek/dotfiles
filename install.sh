#!/bin/bash

########
# nvim #
########

mkdir -p "$XDG_CONFIG_HOME/nvim"
mkdir -p "$XDG_CONFIG_HOME/nvim/undo"

ln -sf "$DOTFILES/nvim/init.vim" "$XDG_CONFIG_HOME/nvim"

rm -rf "$XDG_CONFIG_HOME/X11"
ln -s "$DOTFILES/X11" "$XDG_CONFIG_HOME"

# install neovim plugin manager
[ ! -f "$DOTFILES/nvim/autoload/plug.vim" ] \
    && curl -fLo "$DOTFILES/nvim/autoload/plug.vim" --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

mkdir -p "$XDG_CONFIG_HOME/nvim/autoload"
ln -sf "$DOTFILES/nvim/autoload/plug.vim" "$XDG_CONFIG_HOME/nvim/autoload/plug.vim"

# Install (or update) all the plugins
nvim --noplugin +PlugUpdate +qa

######
# i3 #
######

rm -rf "$XDG_CONFIG_HOME/i3"
ln -s "$DOTFILES/i3" "$XDG_CONFIG_HOME"

#######
# zsh #
#######

mkdir -p "$XDG_CONFIG_HOME/zsh"
ln -sf "$DOTFILES/zsh/.zshenv" "$HOME"
ln -sf "$DOTFILES/zsh/.zshrc" "$XDG_CONFIG_HOME/zsh"
ln -sf "$DOTFILES/zsh/aliases" "$XDG_CONFIG_HOME/zsh/aliases"
rm -rf "$XDG_CONFIG_HOME/zsh/external"
ln -sf "$DOTFILES/zsh/external" "$XDG_CONFIG_HOME/zsh"

#########
# Fonts #
#########

mkdir -p "$XDG_DATA_HOME"
cp -rf "$DOTFILES/fonts" "$XDG_DATA_HOME"

#########
# dunst #
# #######

mkdir -p "$XDG_CONFIG_HOME/dunst"
ln -sf "$DOTFILES/dunst/dunstrc" "$XDG_CONFIG_HOME/dunst/dunstrc"

########
# tmux #
########

mkdir -p "$XDG_CONFIG_HOME/tmux"
ln -sf "$DOTFILES/tmux/tmux.conf" "$XDG_CONFIG_HOME/tmux/tmux.conf"

[ ! -d "$XDG_CONFIG_HOME/tmux/plugins" ] \
&& git clone https://github.com/tmux-plugins/tpm \
"$XDG_CONFIG_HOME/tmux/plugins/tpm"

#########
# tmuxp #
# #######

mkdir -p "$XDG_CONFIG_HOME/tmuxp"
ln -sf "$DOTFILES/tmuxp/dotfiles.yml" "$XDG_CONFIG_HOME/tmuxp/dotfiles.yml"
ln -sf "$DOTFILES/tmuxp/ranger.yml" "$XDG_CONFIG_HOME/tmuxp/ranger.yml"
ln -sf "$DOTFILES/tmuxp/music.yml" "$XDG_CONFIG_HOME/tmuxp/music.yml"

########
#ranger#
########

rm -rf "$XDG_CONFIG_HOME/ranger"
ln -s "$DOTFILES/ranger" "$XDG_CONFIG_HOME"

########
# cmus #
# ######

ln -sf "$DOTFILES/cmus/autosave" "$XDG_CONFIG_HOME/cmus"
ln -sf "$DOTFILES/cmus/cmushnotify" "$XDG_CONFIG_HOME/cmus"
ln -sf "$DOTFILES/cmus/cmus-feh.sh" "$XDG_CONFIG_HOME/cmus"
ln -sf "$DOTFILES/cmus/update_cmus_lib.sh" "$XDG_CONFIG_HOME/cmus"

########
# rofi #
########

ln -sf "$DOTFILES/rofi/ssserpent.rasi" "$XDG_CONFIG_HOME/rofi"
