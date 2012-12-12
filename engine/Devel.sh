#!/bin/zsh
sed -i 's@\t@    @g' `grep -l -P '\t' devwork.cabal templates/* templates/*/* templates/*/*/*`
~/.cabal/bin/yesod devel
