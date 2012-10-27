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
$db = new PDO('sqlite:tatoeba.sqlite');
$db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

$db->query('
CREATE TABLE IF NOT EXISTS `sentences` (
`_id` INTEGER NOT NULL,
`lang` TEXT,
`text` TEXT,
PRIMARY KEY(`_id`)
);
');
$db->query('
CREATE TABLE IF NOT EXISTS `jpn_indices` (
`sentence_id` INTEGER, 
`text` TEXT,
PRIMARY KEY(`sentence_id`)
);
');
$db->query('
CREATE TABLE IF NOT EXISTS `links` (
`sentence_id` INTEGER, 
`translation_id` INTEGER,
UNIQUE(`sentence_id`, `translation_id`)
);
');

echo "Processing jpn_indices.csv\n";
$i=0;
$f = @fopen('jpn_indices.csv', 'r') or die('Could not find jpn_indices.csv');
$db->beginTransaction();
while( ($line = fgets($f)) !== false)
{
	$cols = preg_split('/\t+/', $line);
	if(count($cols) != 3) 
	{
		echo 'Invalid line: ' . $line . "\n";
		continue;
	}
	$stmt = $db->prepare('INSERT OR REPLACE INTO `jpn_indices` (`sentence_id`, `text`) VALUES (:id, :text)');
	$stmt->bindValue(':id', $cols[0], PDO::PARAM_INT);
	$stmt->bindValue(':text', $cols[2], PDO::PARAM_STR);
	$stmt->execute();
	if((++$i % 10000) == 0)	
	{
		$time = time();
		echo ($time-$time0) . "\t" . $i . "\n";
	}
}
$db->commit();
fclose($f);

echo "Processing links.csv\n";
$i=0;
$f = @fopen('links.csv', 'r') or die('Could not find links.csv');
$db->beginTransaction();
$ids = array();
while( ($line = fgets($f)) !== false)
{
	$cols = preg_split('/\t+/', $line);
	if(count($cols) != 2) 
	{
		echo 'Invalid line: ' . $line . "\n";
		continue;
	}
	$stmt = $db->prepare('INSERT INTO `links` (`sentence_id`, `translation_id`) VALUES (:sentence_id, :translation_id)');
	$stmt->bindValue(':sentence_id', $cols[0], PDO::PARAM_INT);
	$stmt->bindValue(':translation_id', $cols[1], PDO::PARAM_INT);
	$stmt->execute();
	if((++$i % 10000) == 0)	
	{
		$time = time();
		echo ($time-$time0) . "\t" . $i . "\n";
	}
}
$db->commit();
fclose($f);

echo "Processing sentences.csv\n";
$i = 0;
$f = @fopen('sentences.csv', 'r') or die('Could not find sentences.csv');
$db->beginTransaction();
while( ($line = fgets($f)) !== false)
{
	$cols = preg_split('/\t+/', $line);
	if(count($cols) != 3) 
	{
		echo 'Invalid line: ' . $line . "\n";
		continue;
	}
	$stmt = $db->prepare('INSERT INTO `sentences` (`_id`, `lang`, `text`) VALUES (:id, :lang, :text)');
	$stmt->bindValue(':id', $cols[0], PDO::PARAM_INT);
	$stmt->bindValue(':lang', $cols[1], PDO::PARAM_STR);
	$stmt->bindValue(':text', $cols[2], PDO::PARAM_STR);
	$stmt->execute();
	if((++$i % 10000) == 0)	
	{
		$time = time();
		echo ($time-$time0) . "\t" . $i . "\n";
	}
}
$db->commit();
fclose($f);

?>
