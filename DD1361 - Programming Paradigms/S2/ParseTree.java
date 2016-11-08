
import java.util.List;

/**
 * 
 * Syntax trädet för Leonardo-språket
 * 
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 *
 */

abstract class ParseTree {
	abstract public void evaluate(Leonardo obj);
}

/*
 * En nod som innehåller expression
 * 
 */

class ExprNode extends ParseTree {
	List<ParseTree> list;
	
	public ExprNode(List<ParseTree> list) {
		this.list = list;
	}

	public void evaluate(Leonardo obj) {
		for (ParseTree in : list) 
			in.evaluate(obj);
	}
}

abstract class InstructionNode extends ParseTree {}

/**
 * 
 * En nod som innehåller "move" instruktion
 *
 */

class MoveNode extends InstructionNode {
	TokenType type;
	int units;
	
	public MoveNode(TokenType type, int units) {
		this.type = type;
		this.units = units;
	}
	
	public void evaluate(Leonardo obj) {
		if (type == TokenType.FORW)
			obj.moveForwards(units);
		else 
			obj.moveBackwards(units);
	}
}

/**
 * 
 * En nod som innehåller "turn" instruktion
 *
 */

class TurnNode extends InstructionNode {
	TokenType type;
	int units;
	
	public TurnNode(TokenType type, int units) {
		this.type = type;
		this.units = units;
	}
	
	public void evaluate(Leonardo obj) {
		if (type == TokenType.LEFT)
			obj.turnLeft(units);
		else 
			obj.turnRight(units);
	}
}

/**
 * 
 * En nod som innehåller "pen" instruktion
 *
 */

class PenNode extends InstructionNode {
	TokenType type;
	
	public PenNode(TokenType type) {
		this.type = type;
	}
	
	public void evaluate(Leonardo obj) {
		if (type == TokenType.UP)
			obj.movePenUp();
		else 
			obj.movePenDown();
	}
}

/**
 * 
 * En nod som innehåller "color" instruktion
 *
 */

class ColorNode extends InstructionNode {
	String data;
	
	public ColorNode(String data) {
		this.data = data;
	}
	
	public void evaluate(Leonardo obj) {
		obj.changeColor(data);
	}
}

/**
 * 
 * En nod som innehåller "repeat" instruktion
 *
 */

class RepeatNode extends InstructionNode {
	int repeats;
	ParseTree expr;
	
	public RepeatNode(int repeats, ParseTree expr) {
		this.expr = expr;
		this.repeats = repeats;
	}
	
	public void evaluate(Leonardo obj) {
		for (int i = 0; i < repeats; i++) {
			expr.evaluate(obj);
		}
	}
}