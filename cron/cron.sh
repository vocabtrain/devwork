#!/bin/zsh
DIR="$( cd "$( dirname "$0" )" && pwd )"
echo $DIR
source "$DIR/../environment.sh"

#sphinx
indexer --rotate --all --config "$datadir/sphinx.conf" --quiet
[[ -z  `ps -A | grep searchd` ]] && searchd --config "$datadir/sphinx.conf"

#backup
cd "$backupdir"
pg_dump -i -U postgres -F c -b -v -f "$backupdir/dump" devwork
git commit -a -m "Autocommit `date`"

#booklanguage cache
psql -f "$schemedir/cacheBookLanguages.sql" devwork
psql -f "$schemedir/cacheBookLanguages.sql" devwork_test
