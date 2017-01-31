#!/bin/bash

alex src/Lexer.x -o gen/Lexer.hs && happy src/Parser.y -o gen/Parser.hs && stack build && stack exec creole-exe
