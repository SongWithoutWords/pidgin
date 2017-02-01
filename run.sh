#!/bin/bash

echo "Making directory" && mkdir -p gen &&
echo "Generating lexer" && alex src/Lexer.x -o gen/Lexer.hs &&
echo "Generating parser" && happy src/Parser.y -o gen/Parser.hs &&
echo "Building project" && stack build &&
echo "Running project" && stack exec creole-exe
