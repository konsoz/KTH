import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.util.stream.Stream;
/**
 * 
 * Solution of the alphabet spam problem
 * 
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 *
 */

public class Main {
	
	static double whitespace;
	static double lowercase;
	static double uppercase;
	static double other;
	
	
	public static void main(String[] args) {
		
		int length;
		
		try {
			
			// Read the input
			String input = readInput(System.in);
		
			// Delete the newline..
			input = input.replace("\n", "");
			
			// Length of the input
			length = input.length();
		
			// Create stream
			Stream<Character> chars = input.chars().mapToObj(i -> (char)i);
			
			// Process every character in the stream
			chars.forEach(
					c -> checkChar(c)
					);
			
			// Calculate ratio
			double whitespaceratio = whitespace / length;
			double lowercaseratio = lowercase / length;
			double uppercaseratio = uppercase / length;
			double otheratio = other / length;
		
			// Print it :)
			
			System.out.println(whitespaceratio);
			System.out.println(lowercaseratio);
			System.out.println(uppercaseratio);
			System.out.println(otheratio);
			
			
	} catch (IOException e) {
			
		}
	}
	
	/**
	 * 
	 * Checks whenever a charachter is a whitespace, lowercase, uppercase or other.
	 * 
	 * @param c
	 */
	
	public static void checkChar(char c) {
		if(c == '_') {
			whitespace++;
		} else if (Character.isLowerCase(c)) {
			lowercase++;
		} else if(Character.isUpperCase(c)) {
			uppercase++;
		} else {
			other++;
		}
	}
	
	/**
	 * Reads the input from standard in
	 * @param f - Input stream
	 * @return - Input as a string
	 * @throws java.io.IOException
	 */
	
	private static String readInput(InputStream f) throws java.io.IOException {
		Reader stdin = new InputStreamReader(f);
		StringBuilder buf = new StringBuilder();
		char input[] = new char[1024];
		int read = 0;
		while ((read = stdin.read(input)) != -1) {
			buf.append(input, 0, read);
		}
		return buf.toString();
	}
	
	
}
