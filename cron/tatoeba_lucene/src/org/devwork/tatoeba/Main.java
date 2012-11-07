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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.FieldType;
import org.apache.lucene.document.LongField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.CorruptIndexException;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopScoreDocCollector;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.store.LockObtainFailedException;

public class Main
{

	static public void createSentences(final String sentence_file, final String lucene_sentences_dir) throws IOException
	{

		if(new File(lucene_sentences_dir).isDirectory())
		{
			System.out.println("Directory " + lucene_sentences_dir + " already exists - exiting.");
			return;
		}

		class Index
		{
			final Directory directory;
			final IndexWriter writer;
			final Analyzer analyzer;
			final IndexWriterConfig config;

			Index(final String language) throws CorruptIndexException, LockObtainFailedException, IOException
			{
				directory = FSDirectory.open(new File(lucene_sentences_dir + '/' + language));
				analyzer = LuceneFunctions.getLanguageAnalyzer(language);
				config = new IndexWriterConfig(LuceneFunctions.LUCENE_VERSION, analyzer);
				writer = new IndexWriter(directory, config);
			}

			void close() throws CorruptIndexException, IOException
			{
				writer.close();
			}

		}

		final Map<String, Index> indices = new HashMap<String, Index>();
		final FieldType idType = new FieldType(LongField.TYPE_STORED);
	    idType.setIndexed(false);
	    idType.omitNorms();
	    
		final BufferedReader r = new BufferedReader(new InputStreamReader(new FileInputStream(sentence_file)));
		String l;
		while((l = r.readLine()) != null)
		{
			final String[] cols = l.split("\t");
			if(cols.length != 3) continue;
			final String language = cols[1];
			if(language.isEmpty() || language.charAt(0) == '\\') continue;
			final Document doc = new Document();
			Index index = indices.get(language);
			if(index == null)
			{
				index = new Index(language);
				indices.put(language, index);
			}
		    doc.add(new LongField(Columns.SENTENCE_ID, Long.parseLong(cols[0]), idType));
		    doc.add(new TextField(Columns.TEXT, cols[2], Field.Store.NO));
			index.writer.addDocument(doc);
		}
		r.close();
		final Iterator<Entry<String, Index>> it = indices.entrySet().iterator();
		while(it.hasNext())
			it.next().getValue().close();

	}

	static public List<Long> findExamples(final String lucene_sentences_dir, final String querystring, final String language, final int limit) throws ParseException, IOException
	{
		final Directory index = FSDirectory.open(new File(lucene_sentences_dir, language));
		final Analyzer analyzer = LuceneFunctions.getLanguageAnalyzer(language);
		final Query q = new QueryParser(LuceneFunctions.LUCENE_VERSION, Columns.TEXT, analyzer).parse(querystring);
		final IndexReader reader = DirectoryReader.open(index);
		final IndexSearcher searcher = new IndexSearcher(reader);
		final TopScoreDocCollector collector = TopScoreDocCollector.create(limit, true);
		searcher.search(q, collector);
		final ScoreDoc[] hits = collector.topDocs().scoreDocs;
		final List<Long> answers = new LinkedList<Long>();

		for(int i = 0; i < hits.length; ++i)
		{
			final int docId = hits[i].doc;
			answers.add(Long.parseLong(searcher.doc(docId).get(Columns.SENTENCE_ID)));
		}
		reader.close();
		return answers;
	}

	public static void main(final String[] args) throws IOException, ParseException
	{

		if(args.length != 2)
		{
			System.out.println("Usage: programname sentence.csv directory");
			System.exit(1);
		}
		createSentences(args[0], args[1]);
		final String test = "continuâmes";
		final String language = "fra";
		System.out.println("Testing database on " + test + " in language " + language + ":");
		final List<Long> examples = findExamples(args[1], test, language, 20);
		for(final Long sentence : examples)
		{
			System.out.println(sentence);
		}

	}

}
