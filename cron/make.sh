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

database='devwork'
datadir=/home/niki/data/
scriptdir=/home/niki/devwork/cron
schemedir=/home/niki/devwork/schemes
mkdir -p "$datadir/log"
function dhaskell {
	of="$datadir/`basename $1 .hs`"
	[[ -f "$datadir/$of" ]] && rm "$datadir/$of"
	[[ -f "$datadir/Main.o" ]] && 	rm "$datadir/Main.o" "$datadir/Main.hi"
	ghc -i "$scriptdir/MyQQ.hs" -outputdir "$datadir" -o $of $1
}
dhaskell "$scriptdir/parseTatoeba.hs"
dhaskell "$scriptdir/parseLanguages.hs"
dhaskell "$scriptdir/genSphinxConf.hs"

[[ -f "$datadir/ISO-639-2_utf-8.txt" ]] || wget 'http://www.loc.gov/standards/iso639-2/ISO-639-2_utf-8.txt' -O "$datadir/ISO-639-2_utf-8.txt" 
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

searchd --config "$datadir/sphinx.conf" --stopwait 

./parseTatoeba "$database"
./parseLanguages "$database" > "$datadir/available_languages.txt"
echo 'und@Undefined Language' >> "$datadir/available_languages.txt"
sort -b "$datadir/available_languages.txt" | uniq > "$datadir/available_languages.txt2"
mv "$datadir/available_languages.txt2" "$datadir/available_languages.txt"
cp "$scriptdir/sphinx.conf.head"  "$datadir/sphinx.conf"
./genSphinxConf "$database" >> "$datadir/sphinx.conf"
mkdir -p "$datadir/sphinx"
indexer --config "$datadir/sphinx.conf" --all
searchd --config "$datadir/sphinx.conf" 

psql -U postgres -f "$schemedir/cacheBookLanguages.sql" devwork

cd $scriptdir/tatoeba_lucene
ant -Dsentences "$datadir/sentences.csv" -Doutputdir "$datadir/lucene" run

cd "$datadir/lucene"
mv "$datadir/tatoeba.sqlite" "$datadir/lucene/"
zip -r "$datadir/tatoeba.zip" .

cd "$datadir"
#ftp -invd web403.webbox555.server-home.org  << EOF
#user web403f7 tatoeba
#put tatoeba.zip
#close
#bye
#
#EOF
