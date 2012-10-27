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
$i = 0;
$time = $time0 = time();
$db = new PDO('sqlite:tatoeba.sqlite');
$db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
$dbe = new PDO('sqlite:edict.sqlite');
$dbe->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
$db->query('drop table if exists ruby;');
$db->query('CREATE TABLE `ruby` (`_id` INTEGER NOT NULL, `text` TEXT, PRIMARY KEY(`_id`))');
$stmt = $db->query('SELECT sentences._id AS id, sentences.text AS aline, jpn_indices.text AS bline FROM `jpn_indices` join sentences on sentences._id = jpn_indices.sentence_id');
$db->beginTransaction();
echo "Progressing ruby_table\n";

while(( $index_row = $stmt->fetch(PDO::FETCH_ASSOC)) != null)
{
//	echo "A-Line: " . $index_row['aline'] . "\n";
//	echo "B-Line: " . $index_row['bline'] . "\n";
		
	$changes = preg_split('/ /', $index_row['bline']);
	$cline = '';
	$aline = $index_row['aline'];
	foreach($changes as &$change)
	{
//		echo "\t $change || ";
		$change = trim($change);
		$search = null;
		$furigana = null;
		$edictnum = 0;
		$edict_furigana = null;
		if(preg_match('/\[/', $change) > 0)
		{
			$matches = null;
			preg_match('/(.*)\[([^]]+)\](.*)/', $change, $matches);
			$edictnum = $matches[2];
			$edictnum -= 1;
			$change = $matches[1] . $matches[3];
		}
		if(preg_match('/\{/', $change) > 0)
		{
			$matches = null;
			preg_match('/(.*)\{([^\}]+)\}(.*)/', $change, $matches);
			$search = $matches[2];
			$change = $matches[1] . $matches[3];
		}
		if(preg_match('/\(/', $change) > 0)
		{
			$matches = null;
			preg_match('/(.*)\(([^\)]+)\)(.*)/', $change, $matches);
			$furigana = $matches[2];
			$change = $matches[1] . $matches[3];
		}
		if(preg_match('/\|1/', $change) > 0)
		{
			$furigana = '|1';
			$change = preg_replace('/\|1/', '', $change);
			if($search == null)
				$search = $change;
		}
		if(preg_match('/~$/', $change) > 0)
		{
			$change = preg_replace('/~$/', '', $change);
		}
		if($furigana == null)
		{
		    $edict_stmt = $dbe->prepare('SELECT meaning FROM edict where _id = :change');
			$edict_stmt->bindValue(':change', $change, PDO::PARAM_STR);
			$edict_stmt->execute();
			if(($edict_row = $edict_stmt->fetch()) != null)
			{
				$furigana = $edict_row['meaning'];
				if(preg_match('/;/', $furigana) > 0)
				{
					$furiganas = preg_split('/;/', $furigana);
					if(array_search($search, $furiganas))
						$furigana = null;
				}
					
			}
		}
		if($furigana == $search) $furigana = null;
		if($search == null) $search = $change;
		if($search == $change)
		{
			$change = $furigana;
			$furigana = null;
		}
//		echo "\t Change $change : Furigana $furigana : Search $search : Edict $edictnum \n";
		if($search == null || strlen($search) == null) continue;	
		$oldcline = strstr($aline, $search, true);
		if($oldcline === FALSE) continue;
		
		$cline .= $oldcline;
		$aline = substr(strstr($aline, $search), strlen($search));

		if($furigana == null)
		{
			if($change == null)
				$cline .= $search;
			else
				$cline .= " <ruby><rb>$search</rb><rp>（</rp><rt>$change</rt><rp>）</rp></ruby> ";
		}
		else
			$cline .= " <ruby><ruby>$search<rp>（</rp><rt>$change</rt><rp>）</rp></ruby><rp>（</rp><rt>$furigana</rt><rp>）</rp></ruby> ";
	}
	$cline .= $aline;
#	echo "C-Line: $cline <br>\n";
	$insert_stmt = $db->prepare('INSERT INTO `ruby` (`_id`, `text`) VALUES (:id, :text)');
	$insert_stmt->bindValue(':id', $index_row['id'] , PDO::PARAM_INT);
	$insert_stmt->bindValue(':text', $cline, PDO::PARAM_STR);
	$insert_stmt->execute();

	if((++$i % 1000) == 0)	
	{
		$time = time();
		echo ($time-$time0) . "\t" . $i . "\n";
	}
}
$db->commit();

?>
