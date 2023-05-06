#!/bin/zsh

# script to split individual flac files from cue sheet
# output is <nr>-<SongName>
# run from relevant directory

find . -name "*.cue" -exec sh -c 'exec shnsplit -f "$1" -o flac -t "%n-%t" "${1%.cue}.flac"' _ {} \;
