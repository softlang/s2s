#!/bin/bash

touch queries

cat "$1" | grep CONSTRUCT | cut -f1 | python3 -c "import sys; from urllib.parse import unquote_plus; print(unquote_plus(sys.stdin.read()));" | sed 's/^$/ggggg/' | tr -s '\n' ' ' | sed 's/ggggg/\n/g' | sed 's/<http:\/\/www\.wikidata\.org\/[^>]*\/P31>/a/g' | sort | uniq -u | shuf >> queries
