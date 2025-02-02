#!/bin/bash

# This script installs basic functionality required for setting up the system.
# All the secrets are kept in ~/.password store directory and used by pass to fill
# all necessary config secrets. Please make sure to transfer manually files:
# privkey.asc, pubkey.asc, gpg-ownertrust.txt files to your ~/ directory and
# ssh keys: id_ed25519 and id_ed_25519.pub to your ~/.ssh
# to running command chezmoi --apply.
#
#!/bin/bash
set -e  # Exit on error

echo "Ensuring required dependencies are installed..."
if ! command -v git &>/dev/null; then
    sudo pacman -Sy --noconfirm git
fi

if ! command -v pass &>/dev/null; then
    sudo pacman -Sy --noconfirm pass
fi

if ! command -v gpg &>/dev/null; then
    sudo pacman -Sy --noconfirm gnupg
fi

if ! command -v ssh &>/dev/null; then
    sudo pacman -Sy --noconfirm openssh
fi

# Ensure SSH is set up correctly
SSH_KEY="$HOME/.ssh/id_ed25519"
if [ ! -f "$SSH_KEY" ]; then
    echo "SSH key not found! Please copy your Ed25519 key to ~/.ssh/id_ed25519"
    exit 1
fi

# Ensure correct SSH permissions
chmod 600 ~/.ssh/id_ed25519
chmod 644 ~/.ssh/id_ed25519.pub
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519

# Test SSH connection to GitHub
if ! ssh -o BatchMode=yes -T git@github.com 2>&1 | grep -q "successfully authenticated"; then
    echo "GitHub authentication failed! Make sure your Ed25519 SSH key is added to GitHub."
    exit 1
fi

# Clone the private pass repository into ~/.password-store
GITHUB_PASS_REPO="git@github.com:klozamarek/pass.git"
PASS_DIR="$HOME/.password-store"

if [ ! -d "$PASS_DIR/.git" ]; then
    echo "Cloning private pass repository into $PASS_DIR..."
    git clone "$GITHUB_PASS_REPO" "$PASS_DIR"
else
    echo "Pass repository already exists, pulling latest changes..."
    cd "$PASS_DIR" && git pull origin main
fi

# Restore GPG Public Keys
if [ -f ~/pubkey.asc ]; then
    echo "Importing GPG public keys..."
    gpg --import ~/pubkey.asc
fi

# Restore GPG Private Keys
if [ -f ~/privkey.asc ]; then
    echo "Importing GPG private keys..."
    gpg --import ~/privkey.asc
fi

# Restore GPG Trust Levels
if [ -f ~/gpg-ownertrust.txt ]; then
    echo "Restoring GPG trust levels..."
    gpg --import-ownertrust ~/gpg-ownertrust.txt
fi

# Ensure pass is initialized with the correct GPG key
GPG_KEY=$(gpg --list-secret-keys --keyid-format LONG | awk -F: '/^sec:/ {print $5; exit}')
if [ -n "$GPG_KEY" ]; then
    echo "Ensuring pass is using the correct GPG key: $GPG_KEY"
    pass init "$GPG_KEY"
fi

