#!/bin/bash
# Update version and push to github
if [ "$#" -ne 1 ]; then
  echo "Usage: $0 VERSION" >&2
  exit 1
fi
VERSION="$1"
SED=sed

function update_file_versions {
    for f in stan-mode/stan-mode.el stan-snippets/stan-snippets.el ac-stan/ac-stan.el
    do
        $SED -i -e "s/^;; \+Version:\(.*\) *$/;; Version: $1/i" $f
    done

    # Update  dependencies in stan-snippets and ac-stan
    for f in stan-snippets/stan-snippets.el ac-stan/ac-stan.el
    do
        $SED -i -e "s/(\(stan-mode\|stan-snippets\) \+\"\([0-9]\+\(\.[0-9]\+\)\+\)\")/(\1 \"$1\")/g" $f
    done
}

git ls-files --other --error-unmatch . >/dev/null 2>&1; ec=$?
if test "$ec" = 0
then
    update_file_versions $VERSION
    git commit -a -m "bump version"
    git tag v$VERSION
    git push
    git push --tags
elif test "$ec" = 1
then
    echo "Repository must be clean" >&2
    exit 1
else
    echo "error from ls-files"
fi

