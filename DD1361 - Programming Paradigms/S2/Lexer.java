
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.List;
import java.util.ArrayList;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 * En klass för att göra lexikal analys.
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 */
public class Lexer {
	
	
	private List<Token> tokens;
	private int currentToken;
	private int currentRow;

	// Hjälpmetod som läser in innehållet i en inputstream till en
	// sträng
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


	public Lexer(InputStream in) throws java.io.IOException {
		String input = Lexer.readInput(in);
		input = input.toUpperCase();
		// Ett regex som beskriver hur ett token kan se ut, plus whitespace (som vi här vill ignorera helt)
		Pattern tokenPattern = Pattern.compile(
			  "%.*\\n"						// Kommentar
			+ "|\\."						// Punkt
			+ "|FORW(\\n|\\040|\\t|%.*\\n)"	// Forw
			+ "|BACK(\\n|\\040|\\t|%.*\\n)"	// Back
			+ "|LEFT(\\n|\\040|\\t|%.*\\n)"	// Left
			+ "|RIGHT(\\n|\\040|\\t|%.*\\n)"// Right
			+ "|DOWN"						// Down
			+ "|UP"							// Up
			+ "|COLOR(\\n|\\040|\\t|%.*\\n)"// Color
			+ "|\\#[0-9ABCDEF]{6}"			// Hex color, ex. #123ABC
			+ "|REP(\\n|\\040|\\t|%.*\\n)"	// Rep
			+ "|\""							// "
			+ "|[1-9][0-9]*(\\n|\\040|\\t|\\.|%.*\\n)"			// Nummer
			+ "|\\n"						// New line
			+ "|(\\040|\\t)+"				// Whitespace
		);
		Matcher m = tokenPattern.matcher(input);
		
		int inputPos = 0;
		tokens = new ArrayList<Token>();
		currentToken = 0;
		currentRow = 1;
		
		// Hitta förekomster av tokens/whitespace i indata
		
		while (m.find()) {
			
			// Om matchningen inte börjar där den borde har vi hoppat
			// över något skräp i indata, markera detta som ett
			// "Invalid"-token
			
			if (m.start() != inputPos) {
				tokens.add(new Token(TokenType.INVALID, currentRow));
			}
			// Kolla vad det var som matchade
			if (m.group().equals("FORW\n") || strBeginingCmp(m.group(), "FORW%")) {
				tokens.add(new Token(TokenType.FORW, currentRow));
				currentRow++; }
			else if (strBeginingCmp(m.group(), "FORW")) 
				tokens.add(new Token(TokenType.FORW, currentRow)); 
			else if (m.group().equals("BACK\n") || strBeginingCmp(m.group(), "BACK%")) {
				tokens.add(new Token(TokenType.BACK, currentRow));
				currentRow++; }
			else if (strBeginingCmp(m.group(), "BACK"))
				tokens.add(new Token(TokenType.BACK, currentRow));
			else if (m.group().equals("LEFT\n") || strBeginingCmp(m.group(), "LEFT%")) {
				tokens.add(new Token(TokenType.LEFT, currentRow));
				currentRow++; }
			else if (strBeginingCmp(m.group(), "LEFT"))
				tokens.add(new Token(TokenType.LEFT, currentRow));
			else if (m.group().equals("RIGHT\n") || strBeginingCmp(m.group(), "RIGHT%")) {
				tokens.add(new Token(TokenType.RIGHT, currentRow));
				currentRow++; }
			else if (strBeginingCmp(m.group(), "RIGHT"))
				tokens.add(new Token(TokenType.RIGHT, currentRow));
			else if (m.group().equals("DOWN"))
				tokens.add(new Token(TokenType.DOWN, currentRow));
			else if (m.group().equals("UP"))
				tokens.add(new Token(TokenType.UP, currentRow));
			else if (m.group().equals("COLOR\n") || strBeginingCmp(m.group(), "COLOR%")) {
				tokens.add(new Token(TokenType.COLOR, currentRow));
				currentRow++; }
			else if (strBeginingCmp(m.group(), "COLOR"))
				tokens.add(new Token(TokenType.COLOR, currentRow));
			else if (m.group().equals("REP\n") || strBeginingCmp(m.group(), "REP%")) {
				tokens.add(new Token(TokenType.REP, currentRow));
				currentRow++; }
			else if (strBeginingCmp(m.group(), "REP"))
				tokens.add(new Token(TokenType.REP, currentRow));
			else if (m.group().equals("\""))
				tokens.add(new Token(TokenType.QUOTE, currentRow));
			else if (m.group().equals("\n")) 
				currentRow++; 
			else if (m.group().equals("."))
				tokens.add(new Token(TokenType.DOT, currentRow));
			else if (m.group().charAt(0) == '#')
				tokens.add(new Token(TokenType.LEGITCOLOR, m.group(), currentRow));
			else if (m.group().charAt(0) == '%')  
				currentRow++; 
			else if (Character.isDigit(m.group().charAt(0))) {
				int lastDigit = 0;
				while (Character.isDigit(m.group().charAt(lastDigit))) 
					lastDigit++;
				tokens.add(new Token(TokenType.NUM, Integer.parseInt(m.group().substring(0, lastDigit)), currentRow));
				if (m.group().charAt(lastDigit) == '%')
					currentRow++;
				else if (m.group().charAt(lastDigit) == '\n')
					currentRow++;
				else if (m.group().charAt(lastDigit) == '.')
					tokens.add(new Token(TokenType.DOT, currentRow)); 
			}
			inputPos = m.end();
		}
		// Kolla om det fanns något kvar av indata som inte var ett token
		if (inputPos != input.length()) {
			tokens.add(new Token(TokenType.INVALID, currentRow));
		}
		// Token som signalerar slut på indata
		tokens.add(new Token(TokenType.EOF, currentRow));
	}
	
	private boolean strBeginingCmp(String str1, String str2) {
		for (int i = 0; i < str2.length(); i++) 
			if (str1.charAt(i) != str2.charAt(i))
				return false;
		return true;
	}

	// Kika på nästa token i indata, utan att gå vidare
	public Token peekToken() {
		return tokens.get(currentToken);
	}

	// Hämta nästa token i indata och gå framåt i indata
	public Token nextToken() {
		Token res = peekToken();
		++currentToken;
		return res;
	}
	
	public Token currentToken() {
		return tokens.get(currentToken-1);
	}
	
	public boolean hasMoreTokens() {
		return currentToken < tokens.size();
	}
}