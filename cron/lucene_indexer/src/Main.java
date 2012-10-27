import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.index.CorruptIndexException;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.queryParser.ParseException;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.store.LockObtainFailedException;
import org.apache.lucene.util.Version;

public class Main {
	
	private static void generateIndices(String tatoebaFilesDir, String luceneDir) throws IOException
	{
		final StandardAnalyzer analyzer = new StandardAnalyzer(Version.LUCENE_35);
		final String lucene_sentences_dir = luceneDir + "/sentences/";
		final File sentences = new File(tatoebaFilesDir + "/sentences.csv");
		
		if(new File(lucene_sentences_dir).isDirectory())
		{
			System.out.println("Directory " + lucene_sentences_dir + " already exists - exiting.");
			return;
		}
		
		class Index
		{
			final Directory directory;
			final IndexWriter writer;
			final IndexWriterConfig config = new IndexWriterConfig(Version.LUCENE_35, analyzer);
			Index(String language) throws CorruptIndexException, LockObtainFailedException, IOException
			{
				directory = FSDirectory.open(new File(lucene_sentences_dir + language));
				writer = new IndexWriter(directory, config);
			}
			void close() throws CorruptIndexException, IOException
			{
				writer.close();
			}
				
		}
		
		Map<String, Index> indices = new HashMap<String, Index>();
		
		
		BufferedReader r = new BufferedReader(new InputStreamReader(new FileInputStream(sentences)));
		String l;
		while((l = r.readLine()) != null)
		{
			String[] cols = l.split("\t");
			if(cols.length != 3) continue;
		    Document doc = new Document();
		    final String language = cols[1];
		   // System.out.println(language);
		    Index index = indices.get(language);
		    if(index == null)
		    {
		    	index = new Index(language);
		    	indices.put(language, index);
		    }
		    doc.add(new Field(ExampleSentence._ID, cols[0], Field.Store.YES, Field.Index.NOT_ANALYZED));
		    doc.add(new Field(ExampleSentence.CONTENT, cols[2], Field.Store.YES, Field.Index.ANALYZED));
		    index.writer.addDocument(doc);
		}
		r.close();
		Iterator<Entry<String, Index>> it = indices.entrySet().iterator();
		while(it.hasNext())
			it.next().getValue().close();
	    
	}
	
	
	
	  public static void main(String[] args) throws IOException, ParseException {
		  if(args.length != 2)
		  {
			  System.out.println("Usage: [Directory containing tatoeba\'s csv] [Directory where to put lucene indices]");
			  System.exit(1);
		  }
		  generateIndices(args[0], args[1]);
		  Sentences sent = new Sentences(args[1]);
		  
		  List<ExampleSentence> examples = sent.findExamples("こん", "jpn", 20);
		  for(ExampleSentence sentence : examples)
		  {
			  System.out.println(sentence);
		  }
	  }
}