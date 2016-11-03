package brute.seq.simulation;

import java.awt.Color;
/**
 * 
 * This class represents a body on the Cartesian graph.
 * 
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 */
public class Body {

	private double positionX, positionY;
	private double velocityX, velocityY; 
	private double forceX, forceY;
	private double mass; 
	private final Color color; 

	public Body(double positionX, double positionY, double velocityX, double velocityY, double mass, Color color) {
		this.positionX = positionX;
		this.positionY = positionY;
		this.velocityX = velocityX;
		this.velocityY = velocityY;
		this.mass = mass;
		this.color = color;
	}
	
	public void setPositionX(double newPositionX){
		positionX = newPositionX;
	}
	
	public void setPositionY(double newPositionY){
		positionY = newPositionY;
	}
	
	public void setVelocityX(double newVelocityX){
		velocityX = newVelocityX;
	}
	
	public void setVelocityY(double newVelocityY){
		velocityY = newVelocityY;
	}

	public double getPositionX() {
		return positionX;
	}

	public double getPositionY() {
		return positionY;
	}

	public double getVelocityX() {
		return velocityX;
	}

	public double getVelocityY() {
		return velocityY;
	}

	public double getMass() {
		return mass;
	}

	public double getForceX() {
		return forceX;
	}

	public void setForceX(double forceX) {
		this.forceX = forceX;
	}

	public double getForceY() {
		return forceY;
	}

	public void setForceY(double forceY) {
		this.forceY = forceY;
	}
	
	public Color getColor() {
		return color;
	}
}
