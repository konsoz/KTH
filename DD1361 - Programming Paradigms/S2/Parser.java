

import java.util.ArrayList;
import java.util.List;

import com.sun.org.apache.xerces.internal.util.SynchronizedSymbolTable;

/**
 * En rekursiv medåknings-parser för leonardo-språket
 * 
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 *
 * 
 */
public class Parser {
	private Lexer lexer;

	public Parser(Lexer lexer) {
		this.lexer = lexer;
	}

	public ParseTree parse() throws SyntaxError {
		// Startsymbol är Expr
		ParseTree result = expr();
		// Borde inte finnas något kvar av indata när vi parsat ett uttryck
		Token t = lexer.nextToken();
		if (t.getType() != TokenType.EOF) {
			throw new SyntaxError(t.getRow());
		}
		return result;
	}
	
	private ParseTree expr() throws SyntaxError {
		List<ParseTree> list = new ArrayList<ParseTree>();
		while (lexer.peekToken().getType() == TokenType.FORW ||
			   lexer.peekToken().getType() == TokenType.BACK ||
			   lexer.peekToken().getType() == TokenType.LEFT ||
			   lexer.peekToken().getType() == TokenType.RIGHT ||
			   lexer.peekToken().getType() == TokenType.DOWN ||
			   lexer.peekToken().getType() == TokenType.UP ||
			   lexer.peekToken().getType() == TokenType.COLOR ||
			   lexer.peekToken().getType() == TokenType.REP) { 
			list.add(instruction());
		}
		return new ExprNode(list);
	}
	
	private ParseTree instruction() throws SyntaxError  {
		Token t = lexer.nextToken();
		switch (t.getType()) {
		case FORW:
			if (!chkNumOperator(t.getRow()))
				throw new SyntaxError(lexer.nextToken().getRow()); // if something else then num
			Token forwNumToken = lexer.nextToken();
			if (!chkDotOperator(forwNumToken.getRow()))
				throw new SyntaxError(lexer.nextToken().getRow()); // if something else then dot
			lexer.nextToken(); // ta bort dot
			return new MoveNode(t.getType(), (int)forwNumToken.getData());
		case BACK:
			if (!chkNumOperator(t.getRow()))
				throw new SyntaxError(lexer.nextToken().getRow()); // if something else then num
			Token backNumToken = lexer.nextToken();
			if (!chkDotOperator(backNumToken.getRow()))
				throw new SyntaxError(lexer.nextToken().getRow()); // if something else then dot
			lexer.nextToken(); // ta bort dot
			return new MoveNode(t.getType(), (int)backNumToken.getData());
		case LEFT:
			if (!chkNumOperator(t.getRow()))
				throw new SyntaxError(lexer.nextToken().getRow()); // if something else then num
			Token leftNumToken = lexer.nextToken();
			if (!chkDotOperator(leftNumToken.getRow()))
				throw new SyntaxError(lexer.nextToken().getRow()); // if something else then dot
			lexer.nextToken(); // ta bort dot
			return new TurnNode(t.getType(), (int)leftNumToken.getData());
		case RIGHT:
			if (!chkNumOperator(t.getRow()))
				throw new SyntaxError(lexer.nextToken().getRow()); // if something else then num
			Token rightNumToken = lexer.nextToken();
			if (!chkDotOperator(rightNumToken.getRow()))
				throw new SyntaxError(lexer.nextToken().getRow()); // if something else then dot
			lexer.nextToken(); // ta bort dot
			return new TurnNode(t.getType(), (int)rightNumToken.getData());
		case DOWN:
			if (!chkDotOperator(t.getRow()))
				throw new SyntaxError(lexer.nextToken().getRow()); // if something else then dot
			lexer.nextToken(); //ta bort dot
			return new PenNode(t.getType());
		case UP:
			if (!chkDotOperator(t.getRow()))
				throw new SyntaxError(lexer.nextToken().getRow()); // if something else then dot
			lexer.nextToken(); //ta bort dot
			return new PenNode(t.getType());
		case COLOR:
			if (!chkColorOperator(t.getRow()))
				throw new SyntaxError(lexer.nextToken().getRow()); // if something else then legitcolor 
			Token colorToken = lexer.nextToken();  // om legitcolor
			if (!chkDotOperator(colorToken.getRow()))
				throw new SyntaxError(lexer.nextToken().getRow()); // if something else then dot
			lexer.nextToken(); //ta bort dot
			return new ColorNode((String) colorToken.getData()); // om dot
		case REP:
			if (!chkNumOperator(t.getRow()))
				throw new SyntaxError(lexer.nextToken().getRow()); // if something else then num
			Token repeatsToken = lexer.nextToken(); // antal repeats
			if (checkQuote(repeatsToken.getRow())) { // om quote
				lexer.nextToken(); // delte quoten
				ParseTree expr = expr();
				if (checkQuote(lexer.currentToken().getRow())) {
					lexer.nextToken(); // tar bort quote
					return new RepeatNode((int)repeatsToken.getData(), expr);
				} else {
					throw new SyntaxError(lexer.nextToken().getRow());
				}
			} else {
				ParseTree inst = instruction();
				return new RepeatNode((int)repeatsToken.getData(), inst);
			}
		default:
			throw new SyntaxError(t.getRow());
		}
	}
	
	/*---- Kontrollera någon operator efter ett kommando ----*/
	
	private boolean checkQuote(int lastRow) throws SyntaxError {
		Token t = lexer.peekToken();
		if (t.getType() == TokenType.QUOTE) 
			return true;
		else if (t.getType() == TokenType.EOF)
			throw new SyntaxError(lastRow);
		return false;
	}

	private boolean chkColorOperator(int lastRow) throws SyntaxError { 
		Token t = lexer.peekToken();
		if (t.getType() == TokenType.LEGITCOLOR)
			return true;
		else if (t.getType() == TokenType.EOF)
			throw new SyntaxError(lastRow);
		return false;
	}

	private boolean chkNumOperator(int lastRow) throws SyntaxError {
		Token t = lexer.peekToken();
		if (t.getType() == TokenType.NUM)
			return true;
		else if (t.getType() == TokenType.EOF) 
			throw new SyntaxError(lastRow);
		return false;
	}

	private boolean chkDotOperator(int lastRow) throws SyntaxError {
		Token t = lexer.peekToken();
		if (t.getType() == TokenType.DOT)
			return true;
		else if (t.getType() == TokenType.EOF)
			throw new SyntaxError(lastRow);
		return false;
	}
}