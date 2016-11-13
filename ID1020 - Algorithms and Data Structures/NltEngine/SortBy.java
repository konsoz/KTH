package se.kth.ID1020.NltEngine_ver2;

import java.util.ArrayList;
import java.util.List;

import se.kth.id1020.util.Attributes;
import se.kth.id1020.util.Document;

/*
 * Sorts attributes by properties: relevance, occurrence and popularity. Each of them can be sorted is ascending and descending order.
 * Based on bubble sort algorithm.    
 */

public class SortBy {

	public static void sortByPopularityAsc(List<Document> listToSort) {
		for (int i = listToSort.size() - 1; i >= 0; i--) {
			for (int j = 0; j < i; j++) {
				if (listToSort.get(j).popularity > listToSort
						.get(j + 1).popularity) {
					Document doc = listToSort.get(j);
					listToSort.set(j, listToSort.get(j + 1));
					listToSort.set(j + 1, doc);
				}
			}
		}
	}

	public static void sortByPopularityDesc(List<Document> listToSort) {
		for (int i = listToSort.size() - 1; i >= 0; i--) {
			for (int j = 0; j < i; j++) {
				if (listToSort.get(j).popularity < listToSort
						.get(j + 1).popularity) {
					Document doc = listToSort.get(j);
					listToSort.set(j, listToSort.get(j + 1));
					listToSort.set(j + 1, doc);
				}
			}
		}
	}

	public static void sortByRelevanceAsc(List<Document> listToSort) {
		for (int i = listToSort.size() - 1; i >= 0; i--) {
			for (int j = 0; j < i; j++) {
				int rel = 0;
				int rel2 = 0;
				for (Document document : listToSort) {
					if (document == listToSort.get(j)) {
						rel++;
					}
					if (document == listToSort.get(j + 1)) {
						rel2++;
					}
				}
				if (rel > rel2) {
					Document tmp = listToSort.get(j);
					listToSort.set(j, listToSort.get(j + 1));
					listToSort.set(j + 1, tmp);
				}
			}
		}
	}

	public static void sortByRelevanceDesc(List<Document> listToSort) {
		for (int i = listToSort.size() - 1; i >= 0; i--) {
			for (int j = 0; j < i; j++) {
				int rel = 0;
				int rel2 = 0;
				for (Document document : listToSort) {
					if (document == listToSort.get(j)) {
						rel++;
					}
					if (document == listToSort.get(j + 1)) {
						rel2++;
					}
				}
				if (rel < rel2) {
					Document tmp = listToSort.get(j);
					listToSort.set(j, listToSort.get(j + 1));
					listToSort.set(j + 1, tmp);
				}
			}
		}
	}

}
