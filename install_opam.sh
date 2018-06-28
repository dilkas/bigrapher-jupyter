#!/usr/bin/env sh -xeu

##
## Usage:
##   curl -sL https://gist.githubusercontent.com/akabe/24979afbf95c4cf4393f589cda997e1b/raw/install_opam.sh | sh -xeu
##
## Variables:
## - $OPAM_PREFIX     Path to install opam
## - $OPAM_VERSION    Version of OPAM to be installed
## - $OCAML_VERSION   Version of OCaml to be installed
##

if which opam; then
    opam update
    opam upgrade -y
else
    mkdir -p "$OPAM_PREFIX/bin"
    curl "https://github.com/ocaml/opam/releases/download/$OPAM_VERSION/opam-$OPAM_VERSION-$(uname -m)-$(uname -s)" \
         -Lo "$OPAM_PREFIX/bin/opam"
    chmod 755 "$OPAM_PREFIX/bin/opam"
    opam init -a -y --comp "$OCAML_VERSION"
fi
