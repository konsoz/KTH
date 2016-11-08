/**
 * 
 * Labb S2, Sköldpaddegrafik. En parser för Leonardo-språket.
 * 
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 */

public class Main {
	public static void main(String args[]) throws java.io.IOException {
		Leonardo l = new Leonardo();
		
		Lexer lexer = new Lexer(System.in);
		Parser parser = new Parser(lexer);
		try {
			ParseTree result = parser.parse();
			result.evaluate(l);
			for (String s : l.lines) {
				System.out.println(s);
			}
		} catch (SyntaxError e) {
			System.out.println(e.getMessage());
		}
	}
}
