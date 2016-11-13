package se.kth.ID1020.NltEngine_ver2;

import java.util.ArrayList;
import java.util.List;

import edu.princeton.cs.algs4.Stack;
import se.kth.id1020.util.Attributes;
import se.kth.id1020.util.Document;

/**
 * 
 * Parses (fully parenthesized) queries expressions using
 *  Dijkstra's two-stack algorithm.
 * @author Konstantin Sozinov
 *
 *
 */

public class Parser {

	public static List<Document> parser(String query, ListOfWords list) {
		String[] str = query.split("\\s+");
		return parse(str,list);
	}
	
	private static List<Document> parse(String [] str ,ListOfWords list) {
        Stack<String> ops = new Stack<>();
        Stack<List<Document>> docs = new Stack<>();
       
        for (int i = str.length-1; i>=0; i--) {
                if      (str[i].equals(")"));
                else if (str[i].equals("+"))    ops.push(str[i]);
                else if (str[i].equals("|"))    ops.push(str[i]);
                else if (str[i].equals("-"))    ops.push(str[i]);
                else if (str[i].equals("(")) {
                        String op = ops.pop();
                        List<Document> doc = docs.pop();
                        if      (op.equals("+")){
                        	List <Document> temp = docs.pop();
                        	List <Document> result = new ArrayList<Document>();
                        	for (Document a : doc) {
                				boolean in = false;
                				for (Document b : temp)
                					if (a == b)
                						in = true;
                				if (in)
                					result.add(a);
                			}
                        	doc = result;
                        }
                        else if (op.equals("|")) doc.addAll(docs.pop());
                        else if (op.equals("-")) {
                        	List <Document> temp = docs.pop();
                        	List <Document> result = new ArrayList<Document>();
	                        	for (Document a : doc) {
	                                boolean in = true;
	                                for (Document b : temp)
                                        if ((a == b))
                                        in = false;
                                if (in)
                                        result.add(a);
                        	}
	                        doc = result;	
                        }
                        docs.push(doc);
                }
                else docs.push(list.search(str[i]));
        }
            
        return docs.pop();
}
	
}
