package barens.parallel.simulation;

import java.awt.Color;

public class Body {

	private static double GRAVITATION = 6.67e-11;
	private static double DT = 1e11;
	
	private Vector position;
	private Vector velocity;
	private Vector force;
	
	private double mass; 
	private final Color color;

	/**
	 * Creates a new body with the given parameters.
	 * 
	 * @param positionX body x coordinate.
	 * @param positionY body y coordinate.
	 * @param velocityX body x velocity.
	 * @param velocityY body y velocity.
	 * @param mass body mass.
	 * @param color body color.
	 */
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

	/**
	 * Returns a new Body object that represents the center-of-mass of the
	 * invoking body and the parameter body b.
	 *
	 * @param b
	 *            the body to add with this Body
	 * @return a new body object representing the center-of-mass of this body and
	 *         the parameter body.
	 */
    public Body add(Body b) {
        double m = getMass() + b.getMass();
        double x = (getPositionX() * getMass() + b.getPositionX() * b.getMass()) / m;
        double y = (getPositionY() * getMass() + b.getPositionY() * b.getMass()) / m;

        return new Body(x, y, 0, 0, m, getColor());
    }

    /**
     * Adds the force acting on this body from the given body.
     * 
     * @param body the body to calculate the force from.
     */
    public void addForce(Body body) {
		double a = Math.pow(body.getPositionX() - getPositionX(), 2);
		double b = Math.pow(body.getPositionY() - getPositionY(),2);
		double distance = Math.sqrt(a+b);
		double magnitute = (GRAVITATION*body.getMass()*getMass()) / Math.pow(distance,2);
		double directionX = body.getPositionX() - getPositionX();
		double directionY = body.getPositionY() - getPositionY();
		setForceX(getForceX() + magnitute * directionX / distance);
		setForceY(getForceY() + magnitute * directionY / distance);
    }

    /**
     * Gets the distance between this body and the given one.
     * 
     * @param b the body to calculate the distance to.
     * @return the distance.
     */
	public double distanceTo(Body b) {
        double dx = getPositionX() - b.getPositionX();
        double dy = getPositionY() - b.getPositionY();
        return Math.sqrt(dx*dx + dy*dy);
	}
	
	/**
	 * Resets the forces acting on this body.
	 */
	public void resetForces() {
		setForceX(0);
		setForceY(0);
	}

	/**
	 * Moves this body according to the forces acting on it.
	 */
    public void update() {
		double deltaVx = getForceX()/getMass()*DT;
		double deltaVy = getForceY()/getMass()*DT;
		double deltaPx = (getVelocityX() + deltaVx/2) * DT;
		double deltaPy = (getVelocityY() + deltaVy/2) * DT;
		
		setVelocityX(getVelocityX()+deltaVx);
		setVelocityY(getVelocityY()+deltaVy);

		setPositionX(getPositionX() + deltaPx);
		setPositionY(getPositionY() + deltaPy);
    }

}
