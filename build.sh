#!/bin/env bash
# should be invoked `. ./build.sh <arg>`
if [[ "$#" -ne 1 ]]; then
    echo "Please supply an argument"
fi

if [[ "$1" == "r" ]]; then
    ghc -Wall Main.hs
    echo "Running Main ..."
    ./Main
fi
if [[ "$1" == "b" ]]; then
    ghc -Wall Main.hs
fi
if [[ "$1" == "c" ]]; then
    echo "Cleaning up ..."
    rm $(fd -Ip ".*\.hi") -f
    rm $(fd -Ip ".*\.o") -f
    rm "Main" -f
fi
