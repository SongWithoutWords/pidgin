#!/bin/bash
echo "Generating parser" && happy -o gen/Parser.hs -iout/parser.info src/Parser.y
