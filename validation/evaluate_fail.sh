#!/bin/bash

# Evaluate the cases that shoud fail.

files="results_should_fail/*.csv"

echo ""

echo -n "_, False:               "
cat $files | grep "False" | wc -l

echo -n "_, True:                "
cat $files | grep "True" | wc -l

echo ""

echo -n "OK                       "
cat $files | grep "OK,False" | wc -l

echo -n "OK_MISSING_IN            "
cat $files | grep "OK_MISSING_IN,False" | wc -l

echo -n "OK_MISSING_OUT           "
cat $files | grep "OK_MISSING_OUT,False" | wc -l

echo -n "MISSING_TARGETS          "
cat $files | grep "MISSING_TARGETS,False" | wc -l

echo -n "MISSING_TARGETS_OUT      "
cat $files | grep "MISSING_TARGETS_OUT,False" | wc -l

echo -n "MISSING_ALL_TARGETS      "
cat $files | grep "MISSING_ALL_TARGETS,False" | wc -l

echo -n "MISSING_ALL_TARGETS_OUT  "
cat $files | grep "MISSING_ALL_TARGETS_OUT,False" | wc -l

echo -n "EMPTY_OUTPUT             "
cat $files | grep "EMPTY_OUTPUT,False" | wc -l

echo -n "EMPTY_INPUT              "
cat $files | grep "EMPTY_INPUT,False" | wc -l

echo -n "QUERY_TIMEOUT            "
cat $files | grep "QUERY_TIMEOUT,False" | wc -l

echo -n "GRAPH_GENERATION_TIMEOUT "
cat $files | grep "GRAPH_GENERATION_TIMEOUT,False" | wc -l
