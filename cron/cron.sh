#!/bin/zsh
DIR="$( cd "$( dirname "$0" )" && pwd )"
source "$DIR/../environment.sh"

#'rotating index .*: success$'
#'^precaching index'
#'^read .* done$'
#sphinx
ignore_warnings=(
'no morphology, index_exact_words=1 has no effect, ignoring'
'Attribute count is 0: switching to none docinfo')
function mute_indexer {
	out="$1"
	for i in $ignore_warnings; do
		out=`echo "$out" | grep -v "$i"`
	done
	[[ -n "$out" ]] && echo "$out"
}


indexer_out=`indexer --rotate --quiet --all --config "$datadir/sphinx.conf"`
mute_indexer "$indexer_out"


SPHINX_PID=`cat $logdir/logsearchd.pid`
if kill -0 "$SPHINX_PID" 2>/dev/null && cat "/proc/$SPHINX_PID/cmdline" | grep -q searchd; then
	;
else
	searchd --config "$datadir/sphinx.conf" >/dev/null
fi

#backup
mkdir -p "$backupdir"
cd "$backupdir"
pg_dump -i -U postgres -F p -b -f "$backupdir/dump" devwork 
git commit --no-status -q -a -m "Autocommit `date`" >/dev/null

#booklanguage cache
psql -q -f "$schemedir/cacheBookLanguages.sql" devwork
psql -q -f "$schemedir/cacheBookLanguages.sql" devwork_test
