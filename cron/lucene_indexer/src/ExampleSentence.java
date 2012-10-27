import org.apache.lucene.document.Document;


public class ExampleSentence
{
	public final static String CONTENT = "content";
	public final static String _ID = "_id";
	
	public final String content;
	public final long id;
	ExampleSentence(Document doc)
	{
		content = doc.get(CONTENT);
		id = Long.parseLong(doc.get(_ID));
	}
	@Override
	public String toString()
	{
		return "" + id  + ", " + content;
	}
}