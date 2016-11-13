package se.kth.ID1302.Schema.Model;

import java.util.ArrayList;
import java.util.List;

public class EventTree<Key extends Comparable<Key>, Value> {

    private static final boolean RED   = true;
    private static final boolean BLACK = false;

    private Node root;

    /**
     * Inner class defining a node in the tree.
     */
    private class Node {
        private Key key;
        private List<Value> val;
        private Node left, right;
        private boolean color;
        private int N;

        public Node(Key key, Value val, boolean color, int N) {
            this.key = key;
            this.val = new ArrayList<Value>();
            this.val.add(val);
            this.color = color;
            this.N = N;
        }
    }

    /**
     * Determines if a node is coloured red.
     * 
     * @param x The node to test.
     * @return	Whether or not the node is red.
     */
    private boolean isRed(Node x) {
        if (x == null) return false;
        return (x.color == RED);
    }

    /**
     * Finds the number of nodes under a specified one.
     * 
     * @param  The node to count under.
     * @return The number of nodes under n.
     */
    public int size(Node n) { 
        if (n == null) return 0;
        return n.N;
    }
    
    /**
     * Finds the size of the tree.
     * 
     * @return The size of the tree.
     */
    public int size() { 
        if (root == null) return 0;
        return root.N;
    }

    /**
     * Determines if the tree is empty.
     * 
     * @return Whether or not the tree is empty.
     */
    public boolean isEmpty() {
        return root == null;
    }
    
    /**
     * Gets the list of values associated with a key.
     * 
     * @param key The key to associate with.
     * @return	  The list the key is associated to.
     */
    public List<Value> get(Key key) { 
    	return get(root, key); 
    }

    /**
     * Recursive function to find the list of values associated with a key.
     * 
     * @param x   The node to search underneath. 
     * @param key The key to search for. 
     * @return	  The resulting list.
     */
    private List<Value> get(Node x, Key key) {
        while (x != null) {
            int cmp = key.compareTo(x.key);
            if      (cmp < 0) x = x.left;
            else if (cmp > 0) x = x.right;
            else              return x.val;
        }
        return null;
    }

    /**
     * Determines if the tree contains a specified key.
     * 
     * @param key The key to search for.
     * @return    Whether or not the tree contains the key. 
     */
    public boolean contains(Key key) {
        return (get(key) != null);
    }
    
    /**
     * Puts a list of values into the tree.
     * 
     * @param key The key to add to.
     * @param val The list to add.
     */
    public void putAll(Key key, List<Value> val) {
    	for (Value v : val)
    		put(key, v);
    }

    /**
     * Puts a values into the tree.
     * 
     * @param key The key to add to.
     * @param val The value to add.
     */
    public void put(Key key, Value val) {
        root = put(root, key, val);
        root.color = BLACK;
    }

    /**
     * Recursive function to put a value to the tree on the node associated with the key.
     * 
     * @param h   Node to look under.
     * @param key Key to look for.
     * @param val Value to add.
     * @return    The, possibly new, root of the tree.
     */
    private Node put(Node h, Key key, Value val) { 
        if (h == null) return new Node(key, val, RED, 1);

        int cmp = key.compareTo(h.key);
        if      (cmp < 0) h.left  = put(h.left,  key, val); 
        else if (cmp > 0) h.right = put(h.right, key, val); 
        else              h.val.add(val);

        if (isRed(h.right) && !isRed(h.left))      h = rotateLeft(h);
        if (isRed(h.left)  &&  isRed(h.left.left)) h = rotateRight(h);
        if (isRed(h.left)  &&  isRed(h.right))     flipColors(h);
        h.N = size(h.left) + size(h.right) + 1;

        return h;
    }

    // the keys between lo and hi, as an Iterable
    public Iterable<Key> keys(Key lo, Key hi) {
        ArrayList<Key> queue = new ArrayList<Key>();
        // if (isEmpty() || lo.compareTo(hi) > 0) return queue;
        keys(root, queue, lo, hi);
        return queue;
    }
    
    // all of the keys, as an Iterable
    public Iterable<Key> keys() {
        return keys(min(), max());
    }
 // the smallest key; null if no such key
    public Key min() {
        if (isEmpty()) return null;
        return min(root).key;
    } 

    // the smallest key in subtree rooted at x; null if no such key
    private Node min(Node x) { 
        // assert x != null;
        if (x.left == null) return x; 
        else                return min(x.left); 
    } 

    // the largest key; null if no such key
    public Key max() {
        if (isEmpty()) return null;
        return max(root).key;
    } 

    // the largest key in the subtree rooted at x; null if no such key
    private Node max(Node x) { 
        // assert x != null;
        if (x.right == null) return x; 
        else                 return max(x.right); 
    } 
    
    // add the keys between lo and hi in the subtree rooted at x
    // to the queue
    private void keys(Node x, ArrayList<Key> queue, Key lo, Key hi) { 
        if (x == null) return; 
        int cmplo = lo.compareTo(x.key); 
        int cmphi = hi.compareTo(x.key); 
        if (cmplo < 0) keys(x.left, queue, lo, hi); 
        if (cmplo <= 0 && cmphi >= 0) queue.add(x.key); 
        if (cmphi > 0) keys(x.right, queue, lo, hi); 
    } 
    
    /**
     * Rotates the supplied node right.
     * 
     * @param h The node to rotate.
     * @return  The node that takes its place.
     */
    private Node rotateRight(Node h) {
        Node x = h.left;
        h.left = x.right;
        x.right = h;
        x.color = x.right.color;
        x.right.color = RED;
        x.N = h.N;
        h.N = size(h.left) + size(h.right) + 1;
        return x;
    }

    /**
     * Rotates the supplied node left.
     * 
     * @param h The node to rotate.
     * @return  The node that takes its place.
     */
    private Node rotateLeft(Node h) {
        Node x = h.right;
        h.right = x.left;
        x.left = h;
        x.color = x.left.color;
        x.left.color = RED;
        x.N = h.N;
        h.N = size(h.left) + size(h.right) + 1;
        return x;
    }

    /**
     * Flips the colour of a node, from red to black or 
     * from black to red.
     * 
     * @param h The node to flip colour of.
     */
    private void flipColors(Node h) {
        h.color = !h.color;
        h.left.color = !h.left.color;
        h.right.color = !h.right.color;
    }
}