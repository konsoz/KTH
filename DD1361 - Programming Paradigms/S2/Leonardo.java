/**
 * 
 * Denna klass representerar Leonardo-objektet med diverse metoder för
 * positionering samt evaluering.
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 */

import java.util.List;
import java.util.ArrayList;

public class Leonardo {
	private double x, y;
	private double direction;
	private String color;
	private boolean isPenDown;
	
	List<String> opHistory = new ArrayList<String>();
	List<String> lines = new ArrayList<String>();
	
	public Leonardo() {
		x = y = 0;
		direction = 0;
		color = "#0000FF";
	}

	/**
	 * 
	 * Ta upp pennan
	 * 
	 */
	
	public void movePenUp() {
		opHistory.add("movePenUp");
		this.isPenDown = false;
		opHistory.add(toString());
	}

	/**
	 * 
	 * Ta ner pennan
	 * 
	 */
	
	public void movePenDown() {
		opHistory.add("movePenDown");
		this.isPenDown = true;
		opHistory.add(toString());
	}

	/**
	 * 
	 * Byta riktning, höger
	 * 
	 * @param degrees - antal grader
	 */
	
	public void turnRight(int degrees) {
		opHistory.add("turnRight " + degrees);
		direction -= degrees;
		opHistory.add(toString());
	}
	
	/**
	 * 
	 * Byta riktning, vänster
	 *  
	 * @param degrees - antal grader
	 */
	
	public void turnLeft(int degrees) {
		opHistory.add("turnLeft " + degrees);
		direction += degrees;
		opHistory.add(toString());
	}
	
	/**
	 * 
	 * Flytta fram
	 * 
	 * @param units - antal enheter
	 */
	
	public void moveForwards(int units) {
		opHistory.add("moveForwards " + units);
		double prevX = x, prevY = y;
		move(units);
		if (isPenDown)
			lines.add(color + " " + prevX + " " + prevY + " " + x + " " + y);
		opHistory.add(toString());
	}
	
	/**
	 * 
	 * Flytta bak
	 * 
	 * @param units - antal enheter
	 */
	
	public void moveBackwards(int units) {
		opHistory.add("moveBackwards " + units);
		double prevX = x, prevY = y;
		move(units*-1);
		if (isPenDown)
			lines.add(color + " " + prevX + " " + prevY + " " + x + " " + y);
		opHistory.add(toString());
	}
	
	/**
	 * 
	 * Dra en linje
	 * 
	 * @param units - antal enheter
	 */
	
	private void move(int units) {
		x = x + units * Math.cos(
				((Math.PI*Math.toRadians(direction))/Math.toRadians(180))
			);
		y = y + units * Math.sin(
				((Math.PI*Math.toRadians(direction))/Math.toRadians(180))
			);
	}
	
	/**
	 * 
	 * Byta färg
	 * 
	 * @param color - färg
	 */
	
	public void changeColor(String color) {
		opHistory.add("changeColor " + color);
		this.color = color;
		opHistory.add(toString());
	}
	
	public String toString() {
		return color + " " + x + " " + y + " " + direction + " " + isPenDown;
	}
}
