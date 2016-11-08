// De olika token-typer vi har i grammatiken
enum TokenType {
	DOWN, LEFT, RIGHT, NUM, INVALID, FORW, DOT, EOF, UP, REP, BACK, COLOR, LEGITCOLOR, QUOTE;
}

/**
 *  Klass för att representera en token
 *
 */
class Token {
	private TokenType type;
	private Object data;
	private int row;
	
	public Token(TokenType type, int row) {
		this.type = type;
		this.data = null;
		this.row  = row;
	}

	public Token(TokenType type, Object data, int row) {
		this.type = type;
		this.data = data;
		this.row  = row;
	}

	public TokenType getType() { return type; }
	public Object getData() { return data; }
	public int getRow() { return row; }

	public String toString(){
		return type.name();
	}
}
