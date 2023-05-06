#!/bin/zsh

# script to tag flac files from cue sheet
# remove first the unsplit .flac file

find . -name "*.cue" -execdir sh -c 'exec cuetag.sh "$1" *.flac' _ {} \;
