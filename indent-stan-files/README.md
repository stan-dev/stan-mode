# Indent Stan Files

The Bash script `indent-stan-file` uses Emacs `stan-mode` to indent a Stan model file.

## Install

1. Download the 
2. If not already installed, install Cask using the instructions [here](http://cask.readthedocs.org/en/latest/).
3. In the `indent-stan-files` directory, run `make build` to install Emacs dependencies.

## Usage

```console
indent-stan-files foo.stan
```

The script `indent-stan-files` can indent multiple files, but indents files in place.

