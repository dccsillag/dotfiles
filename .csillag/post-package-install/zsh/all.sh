#!/bin/sh

chsh -s /bin/zsh "$USER"

# Install ZGen:
git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"
