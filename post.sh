#!/usr/bin/env bash

# Example request for the server component. 
# Server must be running (`./server`).
#
# Note, that this component of s2s is still in development.

curl --header "Content-Type: application/json" \
    --request POST \
    --data '{"query":"CONSTRUCT (n) MATCH (n) WHERE n:Person"}' \
    "http://localhost:8080/parse"

echo ""

curl --header "Content-Type: application/json" \
    --request POST \
    --data '{"query":"MATCH (n) RETURN (n)"}' \
    "http://localhost:8080/parse"

echo ""

curl --header "Content-Type: application/json" \
    --request POST \
    --data '{"query":"CONSTRUCT ...", "iri": "<some-graph>", "args": {}}' \
    "http://localhost:8080/type"
