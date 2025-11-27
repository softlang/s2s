#!/bin/bash

# T

files="results/gen_small*.csv"

echo ""

echo -n "_, True:                 "
cat $files | grep "True" | wc -l

echo -n "_, False:                "
cat $files | grep "False" | wc -l

echo ""

echo -n "OK                       "
cat $files | grep "OK,True" | wc -l

echo -n "OK_MISSING               "
cat $files | grep "OK_MISSING,True" | wc -l

echo -n "MISSING_TARGETS          "
cat $files | grep "MISSING_TARGETS,True" | wc -l

echo -n "MISSING_TARGETS_OUT      "
cat $files | grep "MISSING_TARGETS_OUT,True" | wc -l

echo -n "MISSING_ALL_TARGETS      "
cat $files | grep "MISSING_ALL_TARGETS,True" | wc -l

echo -n "MISSING_ALL_TARGETS_OUT  "
cat $files | grep "MISSING_ALL_TARGETS_OUT,True" | wc -l

echo -n "EMPTY_OUTPUT             "
cat $files | grep "EMPTY_OUTPUT,True" | wc -l

echo -n "EMPTY_INPUT              "
cat $files | grep "EMPTY_INPUT,True" | wc -l

echo -n "QUERY_TIMEOUT            "
cat $files | grep "QUERY_TIMEOUT,True" | wc -l

echo -n "GRAPH_GENERATION_TIMEOUT "
cat $files | grep "GRAPH_GENERATION_TIMEOUT,True" | wc -l
