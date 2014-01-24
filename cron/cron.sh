#!/bin/zsh
DIR="$( cd "$( dirname "$0" )" && pwd )"
echo $DIR
source "$DIR/../environment.sh"

#sphinx
indexer --rotate --quiet --all --config "$datadir/sphinx.conf" --quiet
SPHINX_PID=`cat $logdir/logsearchd.pid`
if kill -0 "$SPHINX_PID" 2>/dev/null && cat "/proc/$SPHINX_PID/cmdline" | grep -q searchd; then
	;
else
	 searchd --config "$datadir/sphinx.conf"
fi

#backup
mkdir -p "$backupdir"
cd "$backupdir"
pg_dump -i -U postgres -F c -b -v -f "$backupdir/dump" devwork
git commit -a -m "Autocommit `date`"

#booklanguage cache
psql -f "$schemedir/cacheBookLanguages.sql" devwork
psql -f "$schemedir/cacheBookLanguages.sql" devwork_test
