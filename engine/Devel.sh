#!/bin/zsh
PATH=$PATH:~/.cabal/bin/
sed -i 's@\t@    @g' `grep -l -P '\t' devwork.cabal templates/* templates/*/* templates/*/*/*`
yesod devel -p 4000
