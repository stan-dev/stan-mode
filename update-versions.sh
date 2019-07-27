#!/bin/bash
# Update version and push to github
if [ "$#" -ne 1 ]; then
  echo "Usage: $0 VERSION" >&2
  exit 1
fi
VERSION="$1"

# I need my sed -i command for in-place editing to work with both GNU sed and BSD/OSX sed
# https://stackoverflow.com/questions/2320564/i-need-my-sed-i-command-for-in-place-editing-to-work-with-both-gnu-sed-and-bsd
# Check sed for GNU sed in which --version works.
# If not try to use gsed (GNU sed in Homebrew).
# In macOS, install with brew install gnu-sed.
if [ sed --version > /dev/null 2>&1 ]
then
    SED=sed
else
    SED=gsed
fi

function update_file_versions {
    for f in stan-mode/*.el stan-mode/create_stan_keywords.py stan-snippets/*.el ac-stan/*.el company-stan/*.el eldoc-stan/*.el flycheck-stan/*.el
    do
        echo Updating the version string in $f
        $SED -i -e "s/^;; \+Version:\(.*\) *$/;; Version: $1/i" $f
        echo Updating dependencies in $f
        $SED -i -e "s/(\(stan-mode\|stan-snippets\) \+\"\([0-9]\+\(\.[0-9]\+\)\+\)\")/(\1 \"$1\")/g" $f
    done
}

git ls-files --other --error-unmatch . >/dev/null 2>&1; ec=$?
if test "$ec" = 0
then
    update_file_versions $VERSION
    # git commit -a -m "bump version"
    # Tag on Github by creating a release
    # git tag v$VERSION
    # git push
    # git push --tags
elif test "$ec" = 1
then
    echo "Repository must be clean" >&2
    exit 1
else
    echo "error from ls-files"
fi
