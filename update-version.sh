#!/bin/bash
# Update version numbers for stan-mode and stan-snippets packages
if [ -z "$1" ]; then
    echo "usage: $0 VERSION"
fi
echo "Bumping stan-mode and stan-snippets version numbers to $1"
sed -r -i \
    -e "s/Version: [0-9]+\\.[0-9]+\\.[0-9]/Version: $1/" \
    -e "s/stan-mode-version \"[0-9]+\\.[0-9]+\\.[0-9]\"/stan-mode-version \"$1\"/" \
    stan-mode.el
sed -r -i \
    -e "s/stan-mode \"[0-9]+\.[0-9]+\.[0-9]+\"/stan-mode \"$1\"/" \
    -e "s/Version: [0-9]+\.[0-9]+\.[0-9]/Version: $1/" \
    stan-snippets.el
