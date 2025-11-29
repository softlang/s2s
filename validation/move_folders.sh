#!/bin/bash

if [ $# -ne 1 ]; then
    echo "Usage: $0 <csv_file>"
    exit 1
fi

csv_file="$1"

if [ ! -f "$csv_file" ]; then
    echo "Error: File $csv_file not found"
    exit 1
fi

# Skip header line, extract first column, move folders
tail -n +2 "$csv_file" | cut -d',' -f1 | while IFS= read -r path; do
    if [ -d "$path" ]; then
        # Create target directory structure if needed
        target_dir="datedone/$(dirname "${path#data/}")"
        mkdir -p "$target_dir"

        # Move the folder
        mv "$path" "datedone/${path#data/}"
        echo "Moved: $path -> datedone/${path#data/}"
    else
        echo "Warning: Directory $path not found, skipping"
    fi
done

