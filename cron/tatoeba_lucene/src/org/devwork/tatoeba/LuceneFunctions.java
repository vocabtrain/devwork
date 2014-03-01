package org.devwork.tatoeba;
/*
Tatoeba - Collection of example sentences for android 
Copyright (C) 2012 Dominik Köppl

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.ar.ArabicAnalyzer;
import org.apache.lucene.analysis.bg.BulgarianAnalyzer;
import org.apache.lucene.analysis.ca.CatalanAnalyzer;
import org.apache.lucene.analysis.cjk.CJKAnalyzer;
import org.apache.lucene.analysis.cz.CzechAnalyzer;
import org.apache.lucene.analysis.da.DanishAnalyzer;
import org.apache.lucene.analysis.de.GermanAnalyzer;
import org.apache.lucene.analysis.el.GreekAnalyzer;
import org.apache.lucene.analysis.en.EnglishAnalyzer;
import org.apache.lucene.analysis.es.SpanishAnalyzer;
import org.apache.lucene.analysis.eu.BasqueAnalyzer;
import org.apache.lucene.analysis.fa.PersianAnalyzer;
import org.apache.lucene.analysis.fi.FinnishAnalyzer;
import org.apache.lucene.analysis.fr.FrenchAnalyzer;
import org.apache.lucene.analysis.gl.GalicianAnalyzer;
import org.apache.lucene.analysis.hi.HindiAnalyzer;
import org.apache.lucene.analysis.hu.HungarianAnalyzer;
import org.apache.lucene.analysis.hy.ArmenianAnalyzer;
import org.apache.lucene.analysis.id.IndonesianAnalyzer;
import org.apache.lucene.analysis.it.ItalianAnalyzer;
import org.apache.lucene.analysis.ja.JapaneseAnalyzer;
import org.apache.lucene.analysis.lv.LatvianAnalyzer;
import org.apache.lucene.analysis.nl.DutchAnalyzer;
import org.apache.lucene.analysis.no.NorwegianAnalyzer;
import org.apache.lucene.analysis.pt.PortugueseAnalyzer;
import org.apache.lucene.analysis.ro.RomanianAnalyzer;
import org.apache.lucene.analysis.ru.RussianAnalyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.analysis.sv.SwedishAnalyzer;
import org.apache.lucene.analysis.th.ThaiAnalyzer;
import org.apache.lucene.analysis.tr.TurkishAnalyzer;
import org.apache.lucene.util.Version;

public class LuceneFunctions
{
	public static final Version LUCENE_VERSION = Version.LUCENE_46;

	public static Analyzer getLanguageAnalyzer(final String language)
	{
		if(language.equals("ara")) return new ArabicAnalyzer(LUCENE_VERSION);
		if(language.equals("hye")) return new ArmenianAnalyzer(LUCENE_VERSION);
		if(language.equals("eus")) return new BasqueAnalyzer(LUCENE_VERSION);
		if(language.equals("bul")) return new BulgarianAnalyzer(LUCENE_VERSION);
		if(language.equals("cat")) return new CatalanAnalyzer(LUCENE_VERSION);
		if(language.equals("zho")) return new CJKAnalyzer(LUCENE_VERSION);
		if(language.equals("jpn")) return new JapaneseAnalyzer(LUCENE_VERSION);
		if(language.equals("kor")) return new CJKAnalyzer(LUCENE_VERSION);
		if(language.equals("ces")) return new CzechAnalyzer(LUCENE_VERSION);
		if(language.equals("dan")) return new DanishAnalyzer(LUCENE_VERSION);
		if(language.equals("dum")) return new DutchAnalyzer(LUCENE_VERSION);
		if(language.equals("eng")) return new EnglishAnalyzer(LUCENE_VERSION);
		if(language.equals("fin")) return new FinnishAnalyzer(LUCENE_VERSION);
		if(language.equals("fra")) return new FrenchAnalyzer(LUCENE_VERSION);
		if(language.equals("glg")) return new GalicianAnalyzer(LUCENE_VERSION);
		if(language.equals("deu")) return new GermanAnalyzer(LUCENE_VERSION);
		if(language.equals("ell")) return new GreekAnalyzer(LUCENE_VERSION);
		if(language.equals("hin")) return new HindiAnalyzer(LUCENE_VERSION);
		if(language.equals("hun")) return new HungarianAnalyzer(LUCENE_VERSION);
		if(language.equals("ind")) return new IndonesianAnalyzer(LUCENE_VERSION);
		if(language.equals("ita")) return new ItalianAnalyzer(LUCENE_VERSION);
		if(language.equals("lav")) return new LatvianAnalyzer(LUCENE_VERSION);
		if(language.equals("nno")) return new NorwegianAnalyzer(LUCENE_VERSION);
		if(language.equals("fas")) return new PersianAnalyzer(LUCENE_VERSION);
		if(language.equals("por")) return new PortugueseAnalyzer(LUCENE_VERSION);
		if(language.equals("ron")) return new RomanianAnalyzer(LUCENE_VERSION);
		if(language.equals("rus")) return new RussianAnalyzer(LUCENE_VERSION);
		if(language.equals("spa")) return new SpanishAnalyzer(LUCENE_VERSION);
		if(language.equals("swe")) return new SwedishAnalyzer(LUCENE_VERSION);
		if(language.equals("tha")) return new ThaiAnalyzer(LUCENE_VERSION);
		if(language.equals("tur")) return new TurkishAnalyzer(LUCENE_VERSION);
		return new StandardAnalyzer(LUCENE_VERSION);

	}

}
