package se.kth.ID1020.NltEngine_ver2;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import se.kth.id1020.Driver;
import se.kth.id1020.TinySearchEngineBase;
import se.kth.id1020.util.Attributes;
import se.kth.id1020.util.Document;

/**
 * 
 * Ordering Parser for a given query. 
 * 
 * @author Konstantin Sozinov
 * 
 */

public class OrderingParser {

	/**
	 * Splits the query into array to prefer parse.
	 * 
	 * @param query
	 * @param list
	 * @return
	 */

	public static List<Document> parser(String query, ListOfWords list, List <Document> doc) {
		String[] str = query.split("\\s+");
		return parse(str, 0, list, doc);
	}

	/**
	 * Recursive read of every action in query. Contains case for different types
	 * of ordering.
	 * 
	 * @param str
	 *            A query handled like String array
	 * @param index
	 *            Index of a actual action to be parsed
	 * @param list
	 *            List of all sentences and its attributes
	 * @param attr
	 *            List of documents to be sorted
	 * @return doc Sorted list of documents
	 */

	private static List<Document> parse(String[] str, int index,
			ListOfWords list, List<Document> doc) {
		if (index > str.length - 1)
			return doc;
		switch (str[index]) {
		case "orderby":
			switch (str[++index]) {
			case "relevance":
				switch (str[++index]) {
				case "asc":
					SortBy.sortByRelevanceAsc(doc);
					break;
				case "desc":
					SortBy.sortByRelevanceDesc(doc);
					break;
				}
				break;
			case "popularity":
				switch (str[++index]) {
				case "asc":
					SortBy.sortByPopularityAsc(doc);
					break;
				case "desc":
					SortBy.sortByPopularityDesc(doc);
					break;
				}
				break;
			}
		default:
			parse(str, index+1,list,doc);
		}
		return doc;
	}
}
