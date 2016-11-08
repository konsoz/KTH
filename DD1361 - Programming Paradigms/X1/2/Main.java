import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Stream;


public class Main {

	public static void main(String[] args) {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		Stream<String> stream = in.lines();
		
		
		Iterator<String> iter = stream.iterator();
		
		String[] firstLine = iter.next().split(" ");
		
		Integer width = Integer.parseInt(firstLine[0]);
		
		int numberOfDividers = Integer.parseInt(firstLine[1]);
		
		String[] secondLine = iter.next().split(" ");
		
		List<String> str = new ArrayList<String>();
		
		str.add("0");
		
		str.addAll(Arrays.asList(secondLine));
		
		str.add(width.toString());
		
	
	
		
		SortedSet<Integer> out = new TreeSet<Integer>(); 
		
		for (int i = 0; i < str.size(); i++) {
			for (int j = i + 1; j < str.size(); j++) {
				int first = Integer.parseInt(str.get(i));
				int second = Integer.parseInt(str.get(j));
				
				int space = second - first;
				
				out.add(space);	
			}
		}
		
		for (Integer integer : out) {
			System.out.print(integer);
			System.out.print(" ");
		}
		
		
	}

	
	
	
	
}
