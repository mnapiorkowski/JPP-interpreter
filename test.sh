#!/bin/bash

NC='\033[0m' # reset color
WHITE_B='\033[1;37m' # white bolded
RED='\033[0;31m'
GREEN='\033[0;32m'

EXT='.ris'

TMP_OUT=$(mktemp)
TMP_ERR=$(mktemp)

for dir in ./tests/{bad,good}/
do
    echo -e "${WHITE_B}--- checking ${dir} ---\n${NC}"
    for file in ${dir}*.ris
    do
        out=${file%$EXT}.out
        err=${file%$EXT}.err
        test_name=$(basename $file $EXT)

        echo -e "${WHITE_B}test $test_name${NC}"

        ./interpreter $file > $TMP_OUT 2> $TMP_ERR

        echo -e "checking stdout..."
        if diff $out $TMP_OUT>/dev/null
        then 
            echo -e "${GREEN}OK${NC}"
        else 
            echo -e "${RED}ERROR${NC}"
        fi

        echo -e "checking stderr..."
        if diff $err $TMP_ERR>/dev/null
        then 
            echo -e "${GREEN}OK${NC}"
        else 
            echo -e "${RED}ERROR${NC}"
        fi
        echo
    done
done