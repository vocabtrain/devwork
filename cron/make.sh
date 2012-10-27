#!/bin/zsh
# Tatoeba for android - Collection of example sentences
# Copyright (C) 2012 by Dominik Köppl
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

datadir=/home/niki/data/
scriptdir=/home/niki/homepage/cron
function dhaskell {
	ghc -i ~/homepage/cron/MyQQ.hs -outputdir ~/data -odir ~/data -o "$datadir/`basename $1 .hs`" $1
}

[[ -f "$datadir/sentences.csv" ]] || wget 'http://tatoeba.org/files/downloads/sentences.csv' -O "$datadir/sentences.csv" 
[[ -f "$datadir/links.csv" ]] || wget 'http://tatoeba.org/files/downloads/links.csv' -O  "$datadir/links.csv"
[[ -f "$datadir/jpn_indices.csv" ]] || wget 'http://tatoeba.org/files/downloads/jpn_indices.csv' -O "$datadir/jpn_indices.csv" 
if [[ ! -f "$datadir/edict" ]]; then
	wget 'http://ftp.monash.edu.au/pub/nihongo/edict.gz' -O "$datadir/edict.gz" && gunzip "$datadir/edict.gz"
fi
[[ -f "$datadir/edict.utf-8" ]] || iconv -feuc-jp -tutf-8 < "$datadir/edict" > "$datadir/edict.utf-8"
cd "$datadir"
[[ -f "$datadir/edict.sqlite" ]] ||  php "$scriptdir/edict_table.php"
[[ -f "$datadir/tatoeba.sqlite" ]] || php "$scriptdir/tatoeba_table.php"
php "$scriptdir/ruby_table.php"
php "$scriptdir/autoruby_supplement.php"
echo 'CREATE TABLE IF NOT EXISTS `android_metadata` (`locale` TEXT DEFAULT "en_US");' | sqlite3 "$datadir/tatoeba.sqlite"
echo 'INSERT INTO `android_metadata` VALUES("en_US");' | sqlite3 "$datadir/tatoeba.sqlite"
echo 'DROP TABLE IF EXISTS `jpn_indices`;' | sqlite3 "$datadir/tatoeba.sqlite"
echo 'VACUUM;' | sqlite3 "$datadir/tatoeba.sqlite"

searchd --config "$scriptdir/sphinx.conf" --stopwait 

runhaskell "$scriptdir/parseTatoeba.hs"
cp "$scriptdir/sphinx.conf.head"  "$scriptdir/sphinx.conf"
runhaskell "$scriptdir/genSphinxConf.hs" >> "$scriptdir/sphinx.conf"
searchd --config "$scriptdir/sphinx.conf" 

psql -U postgres -f cacheBookLanguages.sql devwork

java -cp $scriptdir/lucene_indexer/lib/lucene-core-3.5.0.jar:$scriptdir/lucene_indexer/bin Main "$datadir" "$datadir/lucene"

cd "$datadir/lucene"
mv "$datadir/tatoeba.sqlite" "$datadir/lucene/"
zip -r ../tatoeba.zip .

cd "$datadir"
ftp -invd web403.webbox555.server-home.org  << EOF
user web403f7 tatoeba
put tatoeba.zip
close
bye

EOF
