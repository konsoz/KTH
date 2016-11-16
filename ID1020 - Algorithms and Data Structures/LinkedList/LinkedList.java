package se.kth.ID1020.LinkedList;

/**
 * 
 * Represents a list with nodes, each node have a value and reference to the
 * next node in the chain.
 * 
 */

public class LinkedList<Item extends Comparable<Item>> {

	private int N; // size of the list
	private Node first; // first node

	/**
	 * Constructs an empty list
	 */

	public LinkedList() {
		first = null;
		N = 0;
	}

	/**
	 * Check if a list is empty.
	 * 
	 * @return returns true is the list is not empty, otherwise false
	 */

	public boolean empty() {
		return first == null;
	}

	/**
	 * Returns the content from first node.
	 * 
	 * @return content from first node
	 */

	public Item first() {
		return first.item;
	}

	/**
	 * Returns a link to the next node.
	 * 
	 * @return a link to the next node
	 */

	public Node rest() {
		return first.next;
	}

	/**
	 * Prepend an item to the list.
	 * 
	 * @param An
	 *            item which is going to be prepended
	 */

	public void prepend(Item item) {
		Node oldFirst = first;
		first = new Node();
		first.item = item;
		first.next = oldFirst;
		N++;
	}

	/**
	 * Returns an element with the given index
	 * 
	 * @param index
	 *            of the sought element
	 * @return element on the given index
	 */

	public Item ith(int index) {
		for (Node x = first; x != null; x = x.next) {
			if (index == 0)
				return x.item;
			index--;
		}
		return null;
	}

	/**
	 * Creates string representation of the linked list
	 */

	public String toString() {
		String result = "";
		for (Node x = first; x != null; x = x.next)
			result += "(cons " + (x == null ? " nil" : x.item);
		result += " nil";
		for (int i = 0; i < N; i++)
			result += ")";
		return result;
	}

	/**
	 * Returns the number of items in the list.
	 * 
	 * @return the number of items in the list
	 */

	public int lenght() {
		return N;
	}

	/**
	 * Sorting the list using bubblesort algorithm.
	 */
	
	public int bubbleSort() {
		int r = N - 2;
		int amountSwapps = 0;
		boolean swapped = true;
		while (r >= 0 && swapped == true) {
			swapped = false;
			Node x = first;
			for (int i = 0; i <= r; i++) {
				int cmp = x.item.compareTo(x.next.item);
				if (cmp > 0) {
					amountSwapps++;
					swapped = true;
					swap(x, x.next);
				}
				x = x.next;
			}
			r--;
		}
		return amountSwapps;
	}

	/**
	 * Helper function. Helps to change values in the nodes 
	 * if value in node x is bigger than value in the node next
	 * @param x Value to swap
	 * @param next Value to swap
	 */
	
	private void swap(Node x, Node next) {
		Item tmp = x.item;
		x.item = next.item;
		next.item = tmp;
	}
	
	/**
	 * Counts amount of inversions in list, using brute force algorithm.
	 */

	public void inversionsCountBrute() {
		int inversionsCount = 0;
		for (Node x = first; x.next != null; x = x.next)
			for (Node z = x.next; z.next != null; z = z.next) {
				int cmp = z.item.compareTo(z.next.item);
				if (cmp > 0) {
					inversionsCount++;
				}
			}
		System.out.println("Amount inversions: " + inversionsCount);
	}
	
	/**
	 * 
	 * Merges parts of the array and counts inversions.
	 * 
	 * @param a original array
	 * @param aux auxiliary array used to store temporary data
	 * @param lo low index
	 * @param mid mid index
	 * @param hi high index
	 * @return  inversion in actual subarray
	 */

	private static int merge(Comparable[] a, Comparable[] aux, int lo, int mid, int hi) {
		int inversions = 0;
		
		for (int k = lo; k <= hi; k++) {
			aux[k] = a[k];
		}

		int i = lo, j = mid + 1;
		for (int k = lo; k <= hi; k++) {
			if (i > mid)
				a[k] = aux[j++];
			else if (j > hi)
				a[k] = aux[i++];
			else if ((aux[j].compareTo(aux[i])) < 0 ) {
				a[k] = aux[j++];
				inversions += (mid - i + 1);
			} else
				a[k] = aux[i++];
		}
		return inversions;
	}

	
	/**
	 * 
	 * Returns number of inversion in the subarray. 
	 * Side effect is that b array is rearranged in ascending oreder.
	 * 
	 * @param b original array, represents linked lsit
	 * @param aux  auxiliary array used to store temporary data
	 * @param lo low index
	 * @param hi high index
	 * @return inversions count
	 */
	
	private static int count(Comparable[] b, Comparable[] aux, int lo, int hi) {
		int inversions = 0;
		if (hi <= lo)
			return 0;
		int mid = lo + (hi - lo) / 2;
		inversions += count(b, aux, lo, mid);
		inversions += count(b, aux, mid + 1, hi);
		inversions += merge(b, aux, lo, mid, hi);

		return inversions;
	}

	/*
	 * Copy linked list into an array to perform inversions count with logarithmic 
	 * time complexity NlgN. Uses mergesort algorithm, disvide and conquer method.
	 */
	
	public int inversionsCountLogarithmic() {
		Comparable[] b = new Comparable[this.lenght()];
		Comparable[] aux = new Comparable[this.lenght()];
		for (int i = 0; i < this.lenght(); i++)
			b[i] = ith(i);
		int inversions = count(b, aux, 0, this.lenght() - 1);
		return inversions;
	}

	/**
	 * 
	 * Nested class which presents one node in the list.
	 * 
	 */

	private class Node {
		private Item item;
		private Node next;
	}

}
