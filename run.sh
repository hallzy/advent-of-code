#!/bin/bash

userYear="$1"
userDay="$2"

cwd="$(pwd)"

prefix=''
if [ -n "$userYear" ]; then
    cd "$userYear"
fi

if [ -n "$userDay" ]; then
    cd "$userDay"
fi

echo "Building..."
echo ""
for makefile in $(find . -name Makefile | sort); do
    dir="$(dirname "$makefile")"
    make -C "$dir" build > /dev/null || exit 42
done

echo "Running..."
for makefile in $(find . -name Makefile | sort); do
    dir="$(dirname "$makefile")"

    printf "\n\n"
    echo "==========================="

    time make -C "$dir" run
done
