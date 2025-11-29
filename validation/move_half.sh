#!/bin/bash

source_dir="$1"
dest_dir="$2"

files=("$source_dir"/*)
half=$((${#files[@]} / 2))

mv "${files[@]:0:$half}" "$dest_dir/"
#echo "${files[@]:0:$half}" "$dest_dir/"
echo "Moved $half of ${#files[@]} files"
