#!/bin/bash

echo "Making directories" &&
        mkdir -p gen &&
        mkdir -p out &&
        echo "" > out/parser.info &&

echo "Generating lexer" && alex src/Lexer.x -o gen/Lexer.hs &&
echo "Generating parser" && happy -o gen/Parser.hs -iout/parser.info src/Parser.y &&
echo "Building project" && stack build &&
echo "Running project" && stack exec creole-exe
