#!/bin/bash

echo "Making directories" &&
        mkdir -p gen &&
        mkdir -p out &&
        echo "" > out/parser.info &&

echo "Generating lexer" && alex src/Lexer.x -o gen/Lexer.hs &&
./genParser.sh &&
echo "Building project" && stack build &&
echo "Running project" && stack exec creole-exe
