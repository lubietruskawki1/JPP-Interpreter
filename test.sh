#!/bin/bash

WHITE='\033[0m'
WHITE_BOLD='\033[1;37m'
GREEN_BOLD='\033[1;32m'
RED_BOLD='\033[1;31m'

echo -e "${GREEN_BOLD}Running good examples...\n"

for file in ./examples/good/*.elina
do
    test_name=$(basename $file .elina)
    echo -e "${WHITE_BOLD}test $test_name${WHITE}\n"
    ./interpreter $file
    echo
done

echo -e "${RED_BOLD}Running bad examples...\n"

for file in ./examples/bad/*.elina
do
    test_name=$(basename $file .elina)
    echo -e "${WHITE_BOLD}test $test_name${WHITE}\n"
    ./interpreter $file
    echo
done
