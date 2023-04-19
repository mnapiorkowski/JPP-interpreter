#!/bin/bash

NC='\033[0m' # reset color
WHITE_B='\033[1;37m' # white bolded

for file in ./tests/bad/*.ris
do
    test_name=$(basename $file .ris)
    echo -e "${WHITE_B}test $test_name${NC}\n"
    ./interpreter $file
    echo
done