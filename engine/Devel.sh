#!/bin/zsh
sed -i 's@\t@    @g' `grep -l -P '\t' templates/* templates/*/*`
~/.cabal/bin/yesod devel
