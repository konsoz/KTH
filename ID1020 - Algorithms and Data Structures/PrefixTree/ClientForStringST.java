package se.kth.ID1020.PrefixTree;

public class ClientForStringST {

	public static void main(String[] args) {
	
		StringST Trie = new StringST();
		
		Trie.put("abc");
		Trie.put("ab");
		System.out.println(Trie.distinct("a"));

	}

}
