#!/bin/zsh

# Compress a file
compress() {
    tar cvzf $1.tar.gz $1
}

ftmuxp() {
    if [[ -n $TMUX ]]; then
        return
    fi

    # get the IDs
    ID="$(ls $XDG_CONFIG_HOME/tmuxp | sed -e 's/\.yml$//')"
    if [[ -z "$ID" ]]; then
        tmux new-session
    fi

    create_new_session="Create New Session"

    ID="${create_new_session}\n$ID"
    ID="$(echo $ID | fzf | cut -d: -f1)"

    if [[ "$ID" = "${create_new_session}" ]]; then
        tmux new-session
    elif [[ -n "$ID" ]]; then
        # Rename the current urxvt tab to session name
        printf '\033]777;tabbedx;set_tab_name;%s\007' "$ID"
        tmuxp load "$ID"
    fi
}

# Run man pages in nvim
vman() {
    nvim -c "SuperMan $*"

    if [ "$?" != "0"]; then
        echo "No manual entry for $*"
    fi
}

# Run scratchpad 
scratchpad() {
    "$DOTFILES/zsh/scratchpad.sh"
}

# Run script preventing shell run ranger run shell
ranger() {
    if [ -z "$RANGER_LEVEL" ]; then
        /usr/bin/ranger "$@"
    else
        exit
    fi
}

# Run script to update Arch and others
updatesys() {
    sh $DOTFILES/update.sh
}

# Run script to convert .ape -> .flac
ape2flac() {
    sh $DOTFILES/ape2flac.sh
}

# Run script to split flac files from cue sheet
cue2flac() {
    sh $DOTFILES/cue2flac.sh
}

# Run script to tag flac files from cue sheet
# remove the unsplit flac file FIRST!
tag2flac() {
    sh $DOTFILES/tag2flac.sh
}

