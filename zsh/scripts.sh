#!/bin/zsh

# +-----------------+
# | file management |
# +-----------------+

# file rename to my specific format
renf () {
# Use fzf to choose the file to rename
filename=$(find . -maxdepth 1 -type f -printf '%f\n' | fzf --preview 'echo {}' --prompt='Choose a file to rename: ')

# Get the chosen REF/LST option from the user
echo "1) REF  2) LST  3) NOTE  4) READ ?"
read -r ref_or_lst
case $ref_or_lst in
    1) ref_or_lst="REF" ;;
    2) ref_or_lst="LST" ;;
    3) ref_or_lst="NOTE" ;;
    4) ref_or_lst="READ" ;;
    *) echo "Invalid option. Please choose 1, 2, 3, or 4." && exit 1 ;;
esac

# Get the chosen HME/WRK option from the user
echo "1) HME  2) WRK ?"
read -r hme_or_wrk
case $hme_or_wrk in
    1) hme_or_wrk="HME" ;;
    2) hme_or_wrk="WRK" ;;
    *) echo "Invalid option. Please choose 1 or 2." && exit 1 ;;
esac

# Get the current date in the format YYYY-MM-DD
current_date=$(date +%Y-%m-%d)

# Ask the user if they want to use the original filename or enter a new filename
echo "Use original filename ($filename)? [Y/n]"
read -r use_original_filename

if [[ $use_original_filename =~ ^[Nn]$ ]]; then
    # If the user doesn't want to use the original filename, ask them to enter a new filename
    echo "Enter a new filename:"
    read -r new_filename
else
    # If the user wants to use the original filename, replace all whitespace characters in the filename with underscores
    new_filename="${filename// /_}"
fi

# Get the extension from the original filename
extension="${filename##*.}"

# Prepend the date, REF/LST option, HME/WRK option, and modified filename to the new filename
new_filename="${current_date}_${ref_or_lst}-${hme_or_wrk}_${new_filename%.*}.${extension}"

# Rename the file
mv "$filename" "$new_filename"

echo "File renamed to $new_filename."
}

# replace whitespaces with underscores and make lowercase all filenames in cwd
lowscore () {
for file in *; do 
    mv "$file" "${file// /_}" >/dev/null 2>&1 
    mv "${file// /_}" "$(echo ${file// /_} | tr '[:upper:]' '[:lower:]')" >/dev/null 2>&1; 
done
}

# Compress a file
compress() {
    tar cvzf $1.tar.gzd $1
}

# Open pdf with Zathura
fpdf() {
    result=$(find -type f -name '*.pdf' | fzf --bind "ctrl-r:reload(find -type f -name '*.pdf')" --preview "pdftotext {} - | less")
    [ -n "$result" ] && nohup zathura "$result" &> /dev/null & disown
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

# +--------+
# | backup |
# +--------+

backup() {
    "$DOTFILES/bash/scripts/backup/backup.sh" "-x" "$@" "$DOTFILES/bash/scripts/backup/dir.csv"
}

# +------+
# | tmux |
# +------+

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

# +----------------+
# | sys management |
# +----------------+

# Run script to update Arch and others
updatesys() {
    sh $DOTFILES/update.sh
}

# history statistics
historystat() {
    history 0 | awk '{ if ($2 == "sudo") {print $3} else {print $2} }' | awk -v "FS=|" '{print $1}' | sort | uniq -c | sort -r -n | head -15
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

# +-------+
# | music |
# +-------+

# Run function to convert .ape -> .flac
# function to convert .ape file to .flac file
# run from the relevant directory
ape2flac() {
find . -name "*.ape" -exec sh -c 'exec ffmpeg -i "$1" "${1%.ape}.flac"' _ {} \;
}

# Run function to split flac files from cue sheet
# function to split individual flac files from cue sheet
# output is <nr>-<SongName>
# run from relevant directory
cue2flac() {
find . -name "*.cue" -exec sh -c 'exec shnsplit -f "$1" -o flac -t "%n-%t" "${1%.cue}.flac"' _ {} \;
}

# Run function to tag flac files from cue sheet
# function to tag flac files from cue sheet
# remove the unsplit flac file FIRST!
tag2flac() {
    echo "please remove the unsplit (large) .flac file FIRST!!"
    find . -name "*.cue" -execdir sh -c 'exec cuetag.sh "$1" *.flac' _ {} \;
} 

# Run function to convert .wav -> flac
# run from relevant directory
wav2flac() {
      find . -name "*.wav" -exec sh -c 'exec ffmpeg -i "$1" "${1%.wav}.flac"' _ {} \;
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
