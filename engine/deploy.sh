#!/bin/zsh
cabal install &&
nohup ~/.cabal/bin/devwork Development --port 3000 &
