#!/usr/bin/env bash

# e - script stops on error (return != 0)
# u - error if undefined variable
# o pipefail - script fails if one of piped command fails
# x - output each line (debug)
set -uo pipefail

function output-help()
{
    echo "Usage :  $0 [options] [--]

    Options:
    -h|help       Display this message
    -v|version    Display script version
    -d|dry-run    Run rsync with --dry-run for test
    -x|delete     Run rsync with --delete for mirroring
    -s|size-only  Run rsync with --size-only (no comparison with timestamps)
    -e|exclude    Path to file with exclusion patterns"
}

function run() {
    # Domyślne opcje dla rsync (kompresja, zachowanie atrybutów itp.)
    local rsync_opts=(-avz)
    local exclude_file=""

    __ScriptVersion="1.0"

    # Obsługa opcji wiersza poleceń
    while getopts ":hvdxse:" opt
    do
        case $opt in
            h|help )
                output-help
                exit 0
                ;;
            v|version )
                echo "$0 -- Version $__ScriptVersion"
                exit 0
                ;;
            d|dry-run )
                rsync_opts+=(--dry-run)
                ;;
            x|delete )
                rsync_opts+=(--delete)
                ;;
            s|size-only )
                rsync_opts+=(--size-only)
                ;;
            e|exclude )
                exclude_file="$OPTARG"
                ;;
            * )
                echo -e "\n  Option does not exist : $OPTARG\n"
                output-help
                exit 1
                ;;
        esac
    done
    shift $((OPTIND-1))

    # Sprawdzamy czy podano plik CSV (ostatni argument)
    if [ "$#" == 0 ]; then
        echo "You need to give a file as last argument"
        exit 1
    fi

    # Wczytujemy opcje wykluczeń, jeśli plik istnieje
    if [ -n "$exclude_file" ] && [ -f "$exclude_file" ]; then
        while read -r pattern; do
            rsync_opts+=(--exclude "$pattern")
        done < "$exclude_file"
    fi

    # Plik CSV z parametrami (src,dest)
    local file="${!#}"

    # Odczytujemy plik linia po linii
    while read -r line; do
        # Pomijamy:
        # - Puste linie
        # - Linijki bez przecinka (skrypt zakłada format "src,dest")
        if [[ -z "$line" ]] || [[ "$line" != *,* ]]; then
            continue
        fi

        # Rozbijamy wiersz po pierwszym przecinku:
        local src="$(eval echo -e "${line%,*}")"
        local dest="$(eval echo -e "${line#*,}")"

        echo "Copying $src to $dest from file $file"

        # Jeśli katalog źródłowy nie istnieje, pomijamy
        if [ ! -d "$src" ]; then
            echo "The directory $src does not exist -- NO BACKUP CREATED"
            continue
        fi

        # Utwórz kopię bazowych opcji dla tej konkretnej linii
        local current_opts=("${rsync_opts[@]}")

        # Sprawdzamy, czy w ścieżce docelowej występuje "/mnt/King" (exFAT)
        if [[ "$dest" == *"/mnt/King"* ]]; then
            echo "Detected exFAT destination (contains /mnt/King). Adjusting rsync options..."

            # Usuwamy -avz z kopii, bo w exFAT nie chcemy przenosić uprawnień
            local new_opts=()
            for opt_item in "${current_opts[@]}"; do
                if [[ "$opt_item" != "-avz" ]]; then
                    new_opts+=("$opt_item")
                fi
            done

            # Zamiast -avz używamy -rltDz (rekurencja, linki, czasy, devices, kompresja)
            new_opts+=("-rltDz")

            # Dodajemy --size-only, jeśli jeszcze nie istnieje
            if [[ ! " ${new_opts[*]} " =~ " --size-only " ]]; then
                new_opts+=("--size-only")
            fi
            # Pomijamy prawa, właścicieli, grupy
            new_opts+=("--no-perms" "--no-owner" "--no-group")

            current_opts=("${new_opts[@]}")
        fi

        # Wyświetlamy komendę
        echo "Final rsync command: rsync ${current_opts[*]} ${src}/ $dest"

        # Uruchamiamy rsync
        rsync "${current_opts[@]}" "${src}/" "$dest" 2> /tmp/errors

        echo "------------------------------------"
    done < "$file"

    printf "ERRORS: \n"
    cat /tmp/errors
}

run "$@"
