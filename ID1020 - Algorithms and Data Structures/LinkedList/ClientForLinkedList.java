package se.kth.ID1020.LinkedList;

/**
 * 
 * A client for testing of linked list class.
 * 
 */

import se.kth.ID020.BinarySearch.BinarySearch;
import edu.princeton.cs.introcs.StdIn;
import edu.princeton.cs.introcs.StdOut;

public class ClientForLinkedList {

	public static void main(String[] args) {

		LinkedList<Integer> list = new LinkedList<Integer>();

		while (!StdIn.isEmpty()) {
			String key = StdIn.readString();
			for (int i = key.length() - 1; i >= 0; i--) {
				list.prepend(Integer.parseInt(Character.toString(key.charAt(i))));

			}
			System.out.println("Before sorting: " + list.toString());
			System.out.println("Count: " + list.inversionsCountLogarithmic());
			System.out.println("Amount swaps performed: " + list.bubbleSort());
			System.out.println("After sorting: " + list.toString());
		}
	}
}
