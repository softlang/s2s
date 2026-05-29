#!/bin/sh

cat queries | sed 's/FILTER (.*) \.//g' | sed 's/SERVICE [^}]*}//g' | sed 's/LIMIT.*//g' | sed 's/\*//g' | sed 's/OPTIONAL {[^}]*}//g' | sed 's/{ {/{/g' | sed 's/} }/}/g' | sed 's/Label//g' | sed 's/\.[[:blank:]]*}/}/g' > clean_queries

