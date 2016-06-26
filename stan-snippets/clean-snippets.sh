#!/bin/bash
gnused() {
    if hash gsed 2>/dev/null; then
        gsed "$@"
    else
        sed "$@"
    fi
}
gnused -i -e '/^;;; Do not edit!/d' "$1"
