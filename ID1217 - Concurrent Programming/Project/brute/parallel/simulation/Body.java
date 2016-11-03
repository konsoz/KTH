package brute.parallel.simulation;

import java.awt.Color;
/**
 * 
 * This class represents a body on the Cartesian graph.
 * 
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 */
public class Body {
	
	private Vector position;
	private Vector velocity;
	private Vector force;
	private double mass;
	private final Color color;


	public Body(double positionX, double positionY, double velocityX, double velocityY, double mass, Color color) {
		this.position = new Vector(positionX, positionY);
		this.velocity = new Vector(velocityX, velocityY);
		this.force = new Vector(0.0, 0.0);
		this.mass = mass;
		this.color = color;
	}
	
	public void setPositionX(double newPositionX){
		position.setX(newPositionX);
	}
	
	public void setPositionY(double newPositionY){
		position.setY(newPositionY);
	}
	
	public void setVelocityX(double newVelocityX){
		velocity.setX(newVelocityX);
	}
	
	public void setVelocityY(double newVelocityY){
		velocity.setY(newVelocityY);
	}

	public double getPositionX() {
		return position.getX();
	}

	public double getPositionY() {
		return position.getY();
	}

	public double getVelocityX() {
		return velocity.getX();
	}

	public double getVelocityY() {
		return velocity.getY();
	}

	public double getMass() {
		return mass;
	}

	public double getForceX() {
		return force.getX();
	}

	public void setForceX(double forceX) {
		this.force.setX(forceX);;
	}

	public double getForceY() {
		return force.getY();
	}

	public void setForceY(double forceY) {
		this.force.setY(forceY);
	}
	
	public Color getColor() {
		return color;
	}

	// convert to string representation formatted nicely
	public String toString() {
		return "" + getPositionX() + ", " + getPositionY() + ", " + getVelocityX() + ", " + getVelocityX() + ", " + mass;
	}

}
