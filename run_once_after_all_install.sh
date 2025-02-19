#!/bin/sh

# Change default shell to zsh
if [ "$(basename "$SHELL")" != "zsh" ]; then
    echo "Changing shell to zsh..."
    chsh -s "$(which zsh)"
else
    echo "Shell is already set to zsh."
fi
