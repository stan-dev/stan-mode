#!/bin/bash
# Update version numbers
if [ "$#" -ne 1 ]; then
  echo "Usage: $0 VERSION" >&2
  exit 1
fi
VERSION="$1"

for f in stan-mode/stan-mode.el stan-snippets/stan-snippets.el ac-stan/ac-stan.el
do
    sed -i -e "s/^;; \+Version:.*$/;; Version: $1/i" $f
done

# Update  dependencies in stan-snippets and ac-stan
for f in stan-snippets/stan-snippets.el ac-stan/ac-stan.el
do
    sed -i -e "s/(\(stan-mode\|stan-snippets\) \+\"[0-9]\+\(\.[0-9]\+\)\+\")/(\1 \"$1\")/g" $f
done

