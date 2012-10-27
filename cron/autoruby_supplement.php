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

function mb_html_entity_decode($string)
{
    if (extension_loaded('mbstring') === true)
    {
    	mb_language('Neutral');
    	mb_internal_encoding('UTF-8');
    	mb_detect_order(array('UTF-8', 'ISO-8859-15', 'ISO-8859-1', 'ASCII'));

    	return mb_convert_encoding($string, 'UTF-8', 'HTML-ENTITIES');
    }

    return html_entity_decode($string, ENT_COMPAT, 'UTF-8');
}

function mb_ord($string)
{
    if (extension_loaded('mbstring') === true)
    {
    	mb_language('Neutral');
    	mb_internal_encoding('UTF-8');
    	mb_detect_order(array('UTF-8', 'ISO-8859-15', 'ISO-8859-1', 'ASCII'));

    	$result = unpack('N', mb_convert_encoding($string, 'UCS-4BE', 'UTF-8'));

    	if (is_array($result) === true)
    	{
    		return $result[1];
    	}
    }

    return ord($string);
}

function mb_chr($string)
{
    return mb_html_entity_decode('&#' . intval($string) . ';');
}

$i = 0;
$time = $time0 = time();
$db = new PDO('sqlite:tatoeba.sqlite');
$db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
$dbe = new PDO('sqlite:edict.sqlite');
$dbe->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
$stmt = $db->query('SELECT sentences._id AS id, sentences.text FROM sentences LEFT JOIN ruby ON ruby._id = sentences._id WHERE ruby._id is null AND sentences.lang = "jpn"');
$db->beginTransaction();
echo "Autogenerating supplement for ruby_table\n";

while(( $index_row = $stmt->fetch(PDO::FETCH_ASSOC)) != null)
{
	#echo $index_row['id'] . " ";
	$text = $index_row['text'];
	$aline = $text = preg_replace('/"/','&quot;', $text);
	$cline = '';

#	echo "A-Line: " . $text . "\n";

	while(strlen($text) > 0)
	{
		$furigana = null;
		$old_furigana = null;
		$firstchar = mb_ord(mb_substr($text,0,1, 'utf-8'));
		if( ($firstchar < 0x3040 || $firstchar > 0x309f) && $firstchar > 127)
		for($part_index = 1; $part_index < strlen($text); ++$part_index)
		{
			$old_furigana = $furigana;
			$part = mb_substr($text, 0, $part_index, 'utf-8');
#			echo "Part: $part \n";
			
			$edict_stmt = $dbe->prepare('SELECT meaning FROM edict_search where _id MATCH :part');
			$edict_stmt->bindValue(':part', preg_replace('/[()]/', ' ', $part), PDO::PARAM_STR);
			$edict_stmt->execute();
			if(($edict_row = $edict_stmt->fetch()) != null)
			{
				$furigana = $edict_row['meaning'];
#				echo " - $furigana \n";
			}
			else break;
#			echo " Furigana: $furigana Old: $old_furigana\n";
		}
		if($old_furigana == null)
		{
			$cline .= mb_substr($text, 0, 1, 'utf-8');
			$text = mb_substr($text, 1, strlen($text), 'utf-8');
		}
		else
		{
			--$part_index;
#			echo "Text: $text\t Furigana: $furigana\tPIndex: $part_index \n";
			$substring = mb_substr($text, 0, $part_index, 'utf-8');

			$edict_stmt = $dbe->prepare('SELECT meaning FROM edict where _id = :substring');
			$edict_stmt->bindValue(':substring', $substring, PDO::PARAM_STR);
			$edict_stmt->execute();
			if(($edict_row = $edict_stmt->fetch()) != null)
			{
				$furigana = $edict_row['meaning'];
				$text = mb_substr($text, $part_index, strlen($text), 'utf-8');
				$cline .= " <ruby><rb>$substring</rb><rp>（</rp><rt>$furigana</rt><rp>）</rp></ruby> ";
#				echo "C-Line Pre: $cline <br>\n";
			}
			else
			{
				if($part_index > 1)
				{
					$text = mb_substr($text, $part_index, strlen($text), 'utf-8');
					$cline .= " <ruby><rb>$substring</rb><rp>（</rp><rt>$old_furigana</rt><rp>）</rp></ruby> ";
#					echo "C-Line Pre: $cline <br>\n";
				}
				else
				{
					$cline .= mb_substr($text, 0, 1, 'utf-8');
					$text = mb_substr($text, 1, strlen($text), 'utf-8');
				}
			}


		}
	}


#	echo "A-Line: $aline <br>\n";
	if(strcmp($aline, $cline) == 0) continue;
#	else echo "C-Line: $cline <br>\n";

	$cline = '<ruby><rb>オートマチック:</rb><rp>（</rp><rt>Automatic:</rt><rp>）</rp></ruby> ' . $cline;

	$insert_stmt = $db->prepare('INSERT INTO `ruby` (`_id`, `text`) VALUES (:id, :text)');
	$insert_stmt->bindValue(':id', $index_row['id'] , PDO::PARAM_INT);
	$insert_stmt->bindValue(':text', $cline, PDO::PARAM_STR);
	$insert_stmt->execute();

	if((++$i % 100) == 0)	
	{
		$time = time();
		echo ($time-$time0) . "\t" . $i . "\n";
		$db->commit();
		$db->beginTransaction();
	}
}

?>
