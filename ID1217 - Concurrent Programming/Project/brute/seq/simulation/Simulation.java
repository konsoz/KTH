package brute.seq.simulation;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
/**
 * 
 * Simulation class, it contains logic for calculating forces and moving the bodies.
 * 
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 *
 */
public class Simulation {
	
	private final List<Body> bodies;
	private static double SOLARMASS = 1.98892e30;
	private static double UNIVERSERADIUS = 1e18;
	private static double GRAVITATION = 6.67e-11;
	private double DT = 1e11;
	private int amountCalculations;
	private List<Long> times;
	private int numSteps;
	
	public Simulation(int amountCalculations) {
		numSteps = amountCalculations;
		this.amountCalculations = amountCalculations;
		bodies = new ArrayList<>();
		times = new ArrayList<>();
	}
	
	/**
	 * 
	 * This class will initialize the bodies.
	 * 
	 * @param amountOfBodies
	 */
	public void initBodies(int amountOfBodies) {

		// Put a heavy body in the middle

		Body body = new Body(0, 0, 0, 0, 1e6 * SOLARMASS, Color.RED);
		bodies.add(body);

		// Initialize bodies
		for (int i = 0; i < amountOfBodies; i++) {
			double positionX = UNIVERSERADIUS * exp(-1.8) * (.5 - Math.random());
			double positionY = UNIVERSERADIUS * exp(-1.8) * (.5 - Math.random());
			double magv = circlev(positionX, positionY);

			double absangle = Math.atan(Math.abs(positionY / positionX));
			double thetav = Math.PI / 2 - absangle;
			double velocityX = -1 * Math.signum(positionY) * Math.cos(thetav) * magv;
			double velocityY = Math.signum(positionX) * Math.sin(thetav) * magv;

			double mass = Math.random() * SOLARMASS * 10 + 1e20;
			int red     = (int) Math.floor(mass*254/(SOLARMASS*10+1e20));
		    int blue   = (int) Math.floor(mass*254/(SOLARMASS*10+1e20));
		    int green    = 255;
		    Color color = new Color(red, green, blue);
			bodies.add(new Body(positionX, positionY, velocityX, velocityY, mass, color));
		}

	}

	public double exp(double lambda) {
		return -Math.log(1 - Math.random()) / lambda;
	}

	/**
	 * 
	 * This method will help to initialize the bodies in circular
	 * orbits around the central mass.
	 *
	 * @param rx  x coordinate
	 * @param ry y coordinate
	 * @return position
	 */

	public double circlev(double rx, double ry) {
		double r2 = Math.sqrt(rx * rx + ry * ry);
		double numerator = (GRAVITATION) * 1e6 * SOLARMASS;
		return Math.sqrt(numerator / r2);
	}
	
	/**
	 * 
	 * This method calculates total force for every pair of bodies.
	 * 
	 */
	public void calculateForces() {
		
		double distance;
		double magnitute;
		
		double directionX;
		double directionY;
		 
		for(int i = 0; i < bodies.size()-1; i++)
			for(int j = i+1; j < bodies.size(); j++){
				Body iBody = bodies.get(i);
				Body jBody = bodies.get(j);
				double a = Math.pow(iBody.getPositionX() - jBody.getPositionX(), 2);
				double b = Math.pow(iBody.getPositionY() - jBody.getPositionY(),2);
				distance = Math.sqrt(a+b);
				magnitute = (GRAVITATION*iBody.getMass()*jBody.getMass()) / Math.pow(distance,2);
				directionX = jBody.getPositionX() - iBody.getPositionX();
				directionY = jBody.getPositionY() - iBody.getPositionY();
				
				iBody.setForceX(iBody.getForceX() + magnitute*directionX/distance);
				jBody.setForceX(jBody.getForceX() - magnitute*directionX/distance);
				iBody.setForceY(iBody.getForceY() + magnitute*directionY/distance);
				jBody.setForceY(jBody.getForceY() - magnitute*directionY/distance);
				 
			}
	}
	
	/**
	 * 
	 * This method will calculate new velocity and position for each body.
	 * 
	 */
	public void moveBodies(){
		for(int i = 0; i<bodies.size();i++){
			Body body = bodies.get(i);
			double deltaVx = body.getForceX()/body.getMass()*DT;
			double deltaVy = body.getForceY()/body.getMass()*DT;
			double deltaPx = (body.getVelocityX() + deltaVx/2) * DT;
			double deltaPy = (body.getVelocityY() + deltaVy/2) * DT;
			
			body.setVelocityX(body.getVelocityX()+deltaVx);
			body.setVelocityY(body.getVelocityY()+deltaVy);
			
			body.setPositionX(body.getPositionX() + deltaPx);
			body.setPositionY(body.getPositionY() + deltaPy);
			
			body.setForceX(0);
			body.setForceY(0);
		}
	}
	
	
	/**
	 * 
	 * Run the simulation, calculate forces and then move the bodies.
	 * Take time for every simulation and exit the application when amount calculation is 0.
	 * 
	 */
	public void simulate() {
		if (amountCalculations == 0) {
			Long totalTime = times
				    .stream()
				    .mapToLong(Long::longValue)
				    .sum();
			System.out.println("Number of bodies: " + bodies.size());
			System.out.println("Discretized time step for calculation, DT " + DT);
			System.out.println("The number of time steps in the simulation: " + numSteps);
			System.out.println("Execution time: " + totalTime * 1e-6 +  " milliseconds");
			System.exit(1);
		}
		long a = System.nanoTime();
		calculateForces();
		moveBodies();
		times.add(System.nanoTime() - a);
		amountCalculations--;
	}
	
	public List<Body> getBodies() {
		return bodies;
	}
	
	public double getUniverseRadius() {
		return UNIVERSERADIUS;
	}
}
