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

# Open pdf with Zathura
fpdf() {
    result=$(find -type f -name '*.pdf' | fzf --bind "ctrl-r:reload(find -type f -name '*.pdf')" --preview "pdftotext {} - | less")
    [ -n "$result" ] && nohup zathura "$result" &> /dev/null & disown
}

# check if the package is installed
# usage `_isInstalled "package_name"`
# output 1 not installed; 0 already installed
_isInstalled() {
    package="$1";
    check="$(sudo pacman -Qs --color always "${package}" | grep "local" | grep "${package} ")";
    if [ -n "${check}" ] ; then
        echo 0; #'0' means 'true' in zsh
        return; #true
    fi;
    echo 1; #'1' means 'false' in zsh
    return; #false
}

# `_install <pkg>`
_install() {
    package="$1";

    # If the package IS installed:
    if [[ $(_isInstalled "${package}") == 0 ]]; then
        echo "${package} is already installed.";
        return;
    fi;

    # If the package is NOT installed:
    if [[ $(_isInstalled "${package}") == 1 ]]; then
        sudo pacman -S "${package}";
    fi;
}

# `_installMany <pkg1> <pkg2> ...`
# Works the same as `_install` above,
# but you can pass more than one package to this one.
_installMany() {
    # The packages that are not installed will be added to this array.
    toInstall=();

    for pkg; do
        # If the package IS installed, skip it.
        if [[ $(_isInstalled "${pkg}") == 0 ]]; then
            echo "${pkg} is already installed.";
            continue;
        fi;

        #Otherwise, add it to the list of packages to install.
        toInstall+=("${pkg}");
    done;

    # If no packages were added to the "${toInstall[@]}" array,
    #     don't do anything and stop this function.
    if [[ "${toInstall[@]}" == "" ]] ; then
        echo "All packages are already installed.";
        return;
    fi;

    # Otherwise, install all the packages that have been added to the "${toInstall[@]}" array.
    printf "Packages not installed:\n%s\n" "${toInstall[@]}";
    sudo pacman -S "${toInstall[@]}";
}

# Internal function to extract any file
_ex() {
    case $1 in
        *.tar.bz2)  tar xjf $1      ;;
        *.tar.gz)   tar xzf $1      ;;
        *.bz2)      bunzip2 $1      ;;
        *.gz)       gunzip $1       ;;
        *.tar)      tar xf $1       ;;
        *.tbz2)     tar xjf $1      ;;
        *.tgz)      tar xzf $1      ;;
        *.zip)      unzip $1        ;;
        *.7z)       7z x $1         ;; # require p7zip
        *.rar)      7z x $1         ;; # require p7zip
        *.iso)      7z x $1         ;; # require p7zip
        *.Z)        uncompress $1   ;;
        *)          echo "'$1' cannot be extracted" ;;
    esac
}
# zsh completion
# Display all autocompleted command in zsh.
# First column: command name Second column: completion function
zshcomp() {
    for command completion in ${(kv)_comps:#-*(-|-,*)}
    do
        printf "%-32s %s\n" $command $completion
    done | sort
}
