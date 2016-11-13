package se.kth.ID1020.PrefixTree;

import java.lang.reflect.UndeclaredThrowableException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import edu.princeton.cs.algs4.Queue;

/**
 * 
 * A Trie or Preﬁx Tree is a associative array tree data structure where the
 * keys are encoded in the structure of the tree (as opposed to being stored at
 * a speciﬁc node) and any node can have a value which would the correspond to
 * the key encoded in the path from that node to the root
 * 
 * @author Konstantin Sozinov
 * 
 */

public class StringST implements Iterable<Map.Entry<String, Integer>> {

	private Node current = new Node();
	private Node root = current;
	private static int R = 256;

	private static class Node {

		private int val = 0;
		private Node[] next = new Node[R];

	}

	/**
	 * Inserts a key into the structure and associate it with a value.
	 * 
	 * @param key
	 */

	public void put(String key) {
		Node node = root;
		for (int i = 0; i < key.length(); i++) {
			int c = key.charAt(i);
			if (node.next[c] == null)
				node.next[c] = new Node();

			node = node.next[c];
		}
		if (node.val > 0)
			node.val += 1;
		else
			node.val = 1;
	}

	/**
	 * 
	 * Returns the associated value for k or 0 if no value is associated.
	 * 
	 * @param key
	 * @return value
	 */

	public int get(String key) {
		Node node = root;
		for (int i = 0; i < key.length(); i++) {
			int c = key.charAt(i);
			if (node.next[c] == null)
				return 0;
			node = node.next[c];
		}
		return node.val;
	}

	/**
	 * Returns the sum of all associated values of the sub-tree starting at k.
	 * 
	 * @param prefix
	 * @return sum of all associated values(amount of times a key have been
	 *         repeated)
	 */

	public int count(String prefix) {
		Node node = root;

		for (int i = 0; i < prefix.length(); i++) {
			node = node.next[(prefix.charAt(i))];
			if (node == null)
				return 0;
		}
		return node.val + count(node);
	}

	private int count(Node node) {
		int sum = 0;
		for (int i = 0; i < node.next.length; i++) {
			if (node.next[i] != null)
				sum += node.next[i].val + count(node.next[i]);
		}
		return sum;
	}

	/**
	 * Returns amount of keys within a specified prefix.
	 * 
	 * @param prefix
	 * @return amount of keys
	 */

	public int distinct(String prefix) {
		Node node = root;
		for (int i = 0; i < prefix.length(); i++) {
			node = node.next[(prefix.charAt(i))];
			if (node == null)
				return 0;
		}
		if (node.val == 0)
			return distinct(node);
		else
			return 1 + distinct(node);

	}

	private int distinct(Node node) {
		int sum = 0;
		for (int i = 0; i < node.next.length; i++) {
			if (node.next[i] != null) {
				if (node.next[i].val != 0)
					sum += 1;
				sum += distinct(node.next[i]);
			}
		}
		return sum;
	}

	/**
	 * Returns a node within a specified prefix
	 * 
	 * @param prefix
	 * @return node
	 */
	public Node getNode(String prefix) {

		Node parent = root;
		for (int i = 0; i < prefix.length(); i++) {
			int c = prefix.charAt(i);
			if (parent.next[c] == null)
				return null;
			parent = parent.next[c];
		}
		return parent;
	}

	
	/**
	 * An iterator which travels around the trie. Uses a StringBuilder to get path to the node.
	 * 
	 */
	
	public Iterator<java.util.Map.Entry<String, Integer>> iterator() {
		return iterator("");
	}

	public Iterator<java.util.Map.Entry<String, Integer>> iterator(
			final String prefix) {
		return new Iterator<Map.Entry<String, Integer>>() {

			StringBuilder path = new StringBuilder("");
			char position = (char) 0;
			Node parent = getNode(prefix);
			Node currentChild = null;

			public boolean hasNext() {
				if (currentChild ==null || hasChild(currentChild)){
					return true;
				} else {
					StringBuilder str = new StringBuilder(path.toString()+position);
					while (str.length() > 0){
						char c = str.charAt(str.length()-1);
						str.deleteCharAt(str.length()-1);
						Node n = getNode(prefix+str.toString());
						if (hasChildAfter(n, c))
							return true;
					}
					return false;
				}
			}

			public java.util.Map.Entry<String, Integer> next() {

				if (currentChild == null) {
					position = findFirstChild(parent);
					currentChild = parent.next[position];
				} else {
					if(hasChild(currentChild)){
						path.append(position);
						parent = currentChild;
						position = findFirstChild(parent);
						currentChild = parent.next[position];
					} else {
						stepBackwards();
					}
				}if (currentChild.val == 0)
					return next();
				return new Entry(prefix + path.toString() + position,
							currentChild.val);
			}

			public void remove() {
				// leave it unimplemented
			}
			
			/**
			 * A bunch of helper functions.
			 */

			/**
			 * Helper function, finds out if a node has child.
			 * @param node
			 * @return true - a node has a child, false otherwise
			 */
			
			private boolean hasChild(Node node) {
				for (int i = 0; i < node.next.length; i++) {
					if (node.next[i] != null)
						return true;
				}
				return false;
			}
			
			/**
			 * Checks if a node has child after first child (To the right of the first child )
			 * @param node
			 * @param link
			 * @return
			 */

			private boolean hasChildAfter(Node node, char link) {
				for (char i = (char) ((link) + 1); i < node.next.length; i++) {
					if (node.next[i] != null)
						return true;
				}
				return false;
			}
			
			/**
			 * Finds link to first child of given parent
			 * @param node
			 * @return a char link to the child
			 */

			private char findFirstChild(Node node) {
				for (char i = 0; i < node.next.length; i++) {
					if (node.next[i] != null)
						return i;
				}
				return ' ';
			}
			
			/**
			 * Finds link to the second child after first child of given parent
			 * @param node
			 * @param link
			 * @return link to the second child after first child
			 */

			private char findFirstChildAfter(Node node, char link) {
				for (char i = (char) ((link) + 1); i < node.next.length; i++) {
					if (node.next[i] != null)
						return i;
				}
				return ' ';
			}
			
			/**
			 * Step backwards in the trie when a branch is ready.
			 */
			
			private void stepBackwards(){
				if(hasChildAfter(parent, position)){
					position = findFirstChildAfter(parent,position);
					currentChild = parent.next[position];
				} else {
					position = path.charAt(path.length()-1);
					currentChild = parent;
					path.deleteCharAt(path.length()-1);
					parent = getNode(prefix + path.toString());
					stepBackwards();
				}
			}
		};
	}
	
	/**
	 * 
	 * An entry which is representing one key and value.
	 * 
	 * @author Konstantin Sozinov
	 *
	 */

	public class Entry implements java.util.Map.Entry<String, Integer> {

		private String key;
		private Integer value;

		public Entry(String key, Integer value) {
			this.key = key;
			this.value = value;
		}

		public String getKey() {
			return this.key;
		}

		public Integer getValue() {
			return this.value;
		}

		public Integer setValue(Integer value) {
			return this.value = value;
		}
	}

}
