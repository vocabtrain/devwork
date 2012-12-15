#!/bin/zsh
cabal install &&
nohup ~/.cabal/bin/devwork Testing >> ../log/testing.log 2>&1 &
