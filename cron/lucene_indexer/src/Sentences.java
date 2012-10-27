import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.queryParser.ParseException;
import org.apache.lucene.queryParser.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopScoreDocCollector;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.Version;



public class Sentences
{
	
	Sentences(String luceneDir)
	{
		this.lucene_sentences_dir = luceneDir + "/sentences/";
	}
	private final String lucene_sentences_dir;
	private final StandardAnalyzer analyzer = new StandardAnalyzer(Version.LUCENE_35);

	List<ExampleSentence> findExamples(String querystring, String language, int limit) throws ParseException, IOException
	{
		final Directory index = FSDirectory.open(new File(lucene_sentences_dir + language));
	    Query q = new QueryParser(Version.LUCENE_35, ExampleSentence.CONTENT, analyzer).parse(querystring);
	    IndexReader reader = IndexReader.open(index); 
	    IndexSearcher searcher = new IndexSearcher(reader);
	    TopScoreDocCollector collector = TopScoreDocCollector.create(limit, true);
	    searcher.search(q, collector);
	    ScoreDoc[] hits = collector.topDocs().scoreDocs;
	    final List<ExampleSentence> answers = new LinkedList<ExampleSentence>();
	    
	    for(int i=0;i<hits.length;++i) {
	      int docId = hits[i].doc;
	      answers.add(new ExampleSentence(searcher.doc(docId)));
	    }
	    searcher.close();
	    return answers;
	}
	
}