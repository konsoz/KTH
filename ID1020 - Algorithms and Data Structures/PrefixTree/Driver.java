package se.kth.ID1020.PrefixTree;

import edu.princeton.cs.introcs.In;

import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

public class Driver {

	public static void main(String[] args) {
		StringST trie = new StringST();
		URL url = ClassLoader.getSystemResource("kap1.txt");
		if (url != null) {
			System.out.println("Reading from: " + url);
		} else {
			System.out.println("Couldn’t find file: kap1.txt");
		}
		In input = new In(url);
		while (!input.isEmpty()) {
			String line = input.readLine().trim();
			String[] words = line.split("(\\. )|:|,|;|!|\\?|( - )|--|(\' )| ");
			String lastOfLine = words[words.length - 1];
			if (lastOfLine.endsWith(".")) {
				words[words.length - 1] = lastOfLine.substring(0,
						lastOfLine.length() - 1);
			}
			for (String word : words) {
				String word2 = word.replaceAll("\"|\\(|\\)", "");
				if (word2.isEmpty()) {
					continue;
				}
				trie.put(word2);
				// System.out.println(word2);

			}
		}

		/**
		 * Top ten words
		 */
		int maxValueInMap;
		int minValueInmap;
		int print;
		Map<String, Integer> map = new HashMap<String, Integer>();
		for (Entry<String, Integer> entry : trie) {
			map.put(entry.getKey(), entry.getValue());
		}

		maxValueInMap = Collections.max(map.values());
		minValueInmap = Collections.min(map.values());
		print = 10;
		
		System.err.println("10 words with the highest frequency: ");

		for (int i = maxValueInMap; i > 0; i--) {
			for (Entry<String, Integer> entry : map.entrySet()) {
				if (entry.getValue() == maxValueInMap && print > 0) {
					System.out.println("Key: " + entry.getKey()
							+ " Frequency: " + entry.getValue());
					print--;
				}
			}
			maxValueInMap--;
		}
		
		System.err.println("10 words with the lowest frequency: ");
		maxValueInMap = Collections.max(map.values());
		minValueInmap = Collections.min(map.values());
		print = 10;
		
		for (int i = minValueInmap; i < maxValueInMap; i++) {
			for (Entry<String, Integer> entry : map.entrySet()) {
				if (entry.getValue() == minValueInmap && print > 0) {
					System.out.println("Key: " + entry.getKey()
							+ " Frequency: " + entry.getValue());
					print--;
				}
			}
			minValueInmap++;
		}
		
		/**
		 * Prefix of length 2 with highest frequency
		 */
		int maxFrequency = 0;
		String prefix = "";
		String str = "";
		for (Character c = 0; c < 256;c++)
			for (Character k = 0; k <256;k++){
				prefix = c.toString()+k.toString();
				int max = trie.distinct(prefix);
					if (max > maxFrequency){
						maxFrequency=max;
						str = prefix;
					}
			}
		System.err.println("Prefix of length 2 with highest frequency: ");
		System.out.println(str);

		/**
		 * Most different words start with
		 */
		
		int mostwords = 0;
		char z = ' ';
		
		for (Character c = 0; c < 256; c++){
			String st = c.toString();
			int max = trie.distinct(st);
			if (max > mostwords){
				mostwords=max;
				z = st.charAt(0); 
			}
		}
		System.err.println("Most different words start with: ");
		System.out.println(z);
	}
}