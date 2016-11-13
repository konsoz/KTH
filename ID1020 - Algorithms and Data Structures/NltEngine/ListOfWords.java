package se.kth.ID1020.NltEngine_ver2;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import se.kth.id1020.util.Attributes;
import se.kth.id1020.util.Document;
import se.kth.id1020.util.Sentence;
import se.kth.id1020.util.Word;

/*
 * Contains all words with its attributes. Uses N time for a simple search.
 * Better way to implement it is Binary Search ST. Takes ~logN time to search since words appends with ordered.ss
 */


public class ListOfWords {

	private Node first;
	private Node last;
	public int N;

	/**
	 * Append one word with its attribute to the list
	 * @param word A word
	 * @param attribute Its attribute
	 */
	
	public void append(Sentence sentence, Attributes attribute) {
		if (first == null) {
			first = last = new Node(sentence, attribute);
		} else {
			last.next = new Node(sentence, attribute);
			last = last.next;
		}
		N++;
	}

	/**
	 * Search for a word and return its attribute.
	 * @param query Seeking word
	 * @return List of attributes satisfying the word
	 */
	
	public List<Document> search(String query) {
		List<Node> listOfWordsSatisfyingQuery = new ArrayList<Node>();
		List<Attributes> listOfAttrSatisfyingQuery = new ArrayList<Attributes>();
		for (Node x = first; x != null; x = x.next) {
			for (int i = 0; i < x.words.size();i++){
				if (x.words.get(i).word.equals(query)) {
				
					listOfWordsSatisfyingQuery.add(x);
				}
			}
		}
		for (Node node : listOfWordsSatisfyingQuery) {
			listOfAttrSatisfyingQuery.add(node.attribute);
		}
		
		List <Document> docs = new ArrayList <Document>();
		
		for (Attributes attributes : listOfAttrSatisfyingQuery) {
			docs.add(attributes.document);
		}
		
		return docs;
	}

	/*
	 * Encapsulates a sentence of words with its attributes
	 */
	
	private class Node {

		private List <Word> words;
		private Attributes attribute;
		private Node next;

		 Node(Sentence sentence, Attributes attribute) {
				this.words = sentence.getWords();
				this.attribute = attribute;
		}
	}
}
