#!/bin/sh

pip3 install pynvim

nvim -c ':PlugInstall | qa'
