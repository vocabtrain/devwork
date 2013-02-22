#!/bin/zsh
DIR="$( cd "$( dirname "$0" )" && pwd )"
echo $DIR
source "$DIR/../environment.sh"

#sphinx
indexer --rotate --all --config "$datadir/sphinx.conf" --quiet

#backup
cd "$backupdir"
pg_dump -i -U postgres -F c -b -v -f "$backupdir/dump" devwork
git -am "Autocommit `date`"

#booklanguage cache
psql -f "$schemedir/cacheBookLanguages.sql" devwork
psql -f "$schemedir/cacheBookLanguages.sql" devwork_test
