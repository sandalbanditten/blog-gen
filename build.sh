#!/bin/env bash
cd src

if [[ "$#" -ne 1 ]]; then
    echo "Please supply an argument"
fi

# TODO: Create "build" function

if [[ "$1" == "r" ]]; then
    ghc -dynamic -Wall -XLambdaCase Main.hs || exit 1
    echo "Running Main ..."
    mv Main ..
    cd ..
    ./Main
    exit 0
fi
if [[ "$1" == "b" ]]; then
    ghc -dynamic -Wall -XLambdaCase Main.hs || exit 1
    mv Main ..
    exit 0
fi
if [[ "$1" == "c" ]]; then
    echo "Cleaning up ..."
    rm $(fd -Ip ".*\.hi") -f
    rm $(fd -Ip ".*\.o") -f
    rm ../Main -f
    exit 0
fi
