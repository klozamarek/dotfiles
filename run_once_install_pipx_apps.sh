#!/bin/sh
set -e

# Ensure pipx is installed via pacman (for Arch-based systems)
if ! command -v pipx &>/dev/null; then
    echo "pipx is not installed. Installing it using pacman..."
    sudo pacman -Sy --noconfirm python-pipx
    pipx ensurepath
fi

# Ensure pipx binary path is in the shell environment
export PATH="$HOME/.local/bin:$PATH"

# List of required pipx packages
packages=(
    "buku"
    "kitti3"
    "neovim-remote"
    "nvr"
    "shell-gpt"
)

# Install each package if not already installed
for package in "${packages[@]}"; do
    if ! pipx list | grep -q "$package"; then
        echo "Installing $package with pipx..."
        pipx install "$package"
    else
        echo "$package is already installed, skipping..."
    fi
done

echo "pipx applications installed successfully."
