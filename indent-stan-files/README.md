# Indent Stan Files

The Bash script `indent-stan-file` uses Emacs `stan-mode` to indent a Stan model file.

## Install

1. Download this directory however you so desire.
2. If not already installed, install Cask using the instructions [here](http://cask.readthedocs.org/en/latest/).
3. In the `indent-stan-files` directory, run `make build` to install Emacs dependencies.

## Usage

The script `indent-stan-files` only indents one model file at a time.
To indent a file in place:
```console
indent-stan-files intputfile.stan
```
To indent a file and save it to another file:
```console
indent-stan-files intputfile.stan outputfile.stan
```
