<?php
/* Tatoeba for android - Collection of example sentences
 * Copyright (C) 2012 by Dominik Köppl
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
error_reporting(E_ALL | E_STRICT);
$time = $time0 = time();
$db = new PDO('sqlite:edict.sqlite');
$db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

$db->query('
CREATE TABLE IF NOT EXISTS `edict` (
`_id` TEXT NOT NULL,
`meaning` TEXT NOT NULL,
PRIMARY KEY(`_id`)
);
');

$db->query('CREATE VIRTUAL TABLE `edict_search` USING fts4 ( `_id`, `meaning`);');


echo "Processing edict\n";
$i = 0;
$f = @fopen('edict.utf-8', 'r') or die('Could not find sentences.csv');
$db->beginTransaction();
$line = fgets($f); // remove first line
$oldid = '';
while( ($line = fgets($f)) !== false)
{
	$cols = preg_split('/ /', $line);
	if(preg_match('/\[/', $line) == 0 || count($cols) < 2) 
	{
//		echo 'Invalid line: ' . $line;
		continue;
	}
	
	$matches = null;
	preg_match('/(.*)\[([^]]+)\](.*)/', $line, $matches);
	$meaning = $matches[2];

	if($oldid == $cols[0])
	{
		$meaning = "; $meaning";
		$stmt = $db->prepare('UPDATE `edict` SET `meaning` = `meaning` || :meaning WHERE `_id` = :id');
		$stmt->bindValue(':id', $cols[0], PDO::PARAM_STR);
		$stmt->bindValue(':meaning', $meaning, PDO::PARAM_STR);
		$stmt->execute();
	}
	else
	{
		$oldid = $cols[0];
		$stmt = $db->prepare('INSERT INTO `edict` (`_id`, `meaning`) VALUES (:id, :meaning)');
		$stmt->bindValue(':id', $cols[0], PDO::PARAM_STR);
		$stmt->bindValue(':meaning', $meaning, PDO::PARAM_STR);
		$stmt->execute();
	}
	if((++$i % 10000) == 0)	
	{
		$time = time();
		echo ($time-$time0) . "\t" . $i . "\n";
	}
}
$db->commit();
$db->query('insert into edict_search select * from edict;');
fclose($f);
?>
