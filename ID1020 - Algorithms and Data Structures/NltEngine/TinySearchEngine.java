package se.kth.ID1020.NltEngine_ver2;

import java.util.ArrayList;
import java.util.List;

import se.kth.ID1020.NltEngine_ver2.OrderingParser;
import se.kth.ID1020.PrefixTree.PrefixToInfix;
import se.kth.id1020.Driver;
import se.kth.id1020.TinySearchEngineBase;
import se.kth.id1020.util.Attributes;
import se.kth.id1020.util.Document;
import se.kth.id1020.util.Sentence;

public class TinySearchEngine implements TinySearchEngineBase {
	
	private ListOfWords list = new ListOfWords();

	@Override
	public String infix(String arg0) {
		return null;
	}

	/**
	 * Adds the sentence with the given attribute to your index.
	 */
	
	public void insert(Sentence arg0, Attributes arg1) {
		list.append(arg0, arg1);
	}

	
	public void postInserts() {
		
	}

	
	public void preInserts() {
		
	}

	/**
	 * returns the list of documents that matches the query
	 */
	public List<Document> search(String arg0) {
		
		List<Document> docs = Parser.parser(arg0, list);
		docs = OrderingParser.parser(arg0, list, docs);
			List<Document> result = new ArrayList<Document>();
			for (Document d : docs){
				if (!(result.contains(d)))
					result.add(d);
		}
		
		return result;
	}

	
	public static void main(String[] args) throws Exception {
		TinySearchEngineBase searchEngine = new TinySearchEngine();
		 Driver.run(searchEngine);
	}
}
