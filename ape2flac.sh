#!/bin/zsh

# script to convert .ape file to .flac file
# run from the relevant directory

find . -name "*.ape" -exec sh -c 'exec ffmpeg -i "$1" "${1%.ape}.flac"' _ {} \;
