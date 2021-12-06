#!/bin/bash

userYear="$1"
userDay="$2"

for makefile in $(find . -name Makefile | sort); do
    dir="$(dirname "$makefile")"
    dirdir="$(dirname "$dir")"
    day="$(basename "$dir")"
    year="$(basename "$dirdir")"

    if [ -n "$userYear" ] && [ "$userYear" -ne "$year" ]; then
        continue;
    fi
    if [ -n "$userDay" ] && [ "$userDay" -ne "$day" ]; then
        continue;
    fi

    make -C "$dir"
done

for main in $(find . -name main | sort); do
    dir="$(dirname "$main")"
    dirdir="$(dirname "$dir")"
    day="$(basename "$dir")"
    year="$(basename "$dirdir")"

    if [ -n "$userYear" ] && [ "$userYear" -ne "$year" ]; then
        continue;
    fi
    if [ -n "$userDay" ] && [ "$userDay" -ne "$day" ]; then
        continue;
    fi

    printf "\n\n"

    echo "$year - $day"
    echo "==========================="
    cd "$dir"
    ./main
    cd - > /dev/null
done
