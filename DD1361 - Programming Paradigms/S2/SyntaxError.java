/**
 * Class to represent syntax error on a specific row.
 */
@SuppressWarnings("serial")
public class SyntaxError extends Exception {
	public SyntaxError(int row) {
		super("Syntaxfel på rad " + row);
	}
}
