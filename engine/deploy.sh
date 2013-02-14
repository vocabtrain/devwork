#!/bin/zsh
cabal configure &&
cabal build &&
nohup ./dist/build/devwork/devwork Testing >> ../log/testing.log 2>&1 &
