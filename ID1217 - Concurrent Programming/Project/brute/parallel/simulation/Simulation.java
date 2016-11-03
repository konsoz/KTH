package brute.parallel.simulation;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.locks.Lock;
/**
 * 
 * Simulation class, it will start worker threads and distribute the work among them.
 * 
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
	private int numberOfWorkers;
	private int amountOfCalculations;
	private Vector[][] forces;
	private List<Long> times = new ArrayList<>();
	private Worker[] workers;
	
	public Simulation(int numberOfWorkers, int numberOfBodies, int amoutOfCalculation) {
		
		bodies = new ArrayList<Body>();
		this.numberOfWorkers = numberOfWorkers;
		this.amountOfCalculations = amoutOfCalculation;
		initBodies(numberOfBodies);
		
		// Initialize forces vector
		forces = new Vector[numberOfWorkers][bodies.size()];
		for (int i = 0; i < forces.length; i++) {
			for (int j = 0; j < forces[i].length; j++) {
				forces[i][j] = new Vector(0.0, 0.0);
			}
		}
	}

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
	
	public double getDT(){
		return DT;
	}
	
	public void simulate(){
		
		// Parallelize the work
		CyclicBarrier barrier = new CyclicBarrier(numberOfWorkers);
		CyclicBarrier lock = new CyclicBarrier(numberOfWorkers + 1);
		
		// Create threads every time we will simulate, this is extremely bad but it is need
		// in order to calculate performance time.
		// Otherwise threads can be created just ones and synchronized with gui with some barrier.
		for (int i = 0; i < numberOfWorkers; i++) 
            new Worker(i, numberOfWorkers, bodies, forces, barrier, lock).start();
        
        try {
        	lock.await(); // Wait for thread creation
    		long a = System.nanoTime();
    		lock.await(); // Wait until all are done
    		long b = System.nanoTime() - a;
    		times.add(b);
		} catch (BrokenBarrierException | InterruptedException e) {
			e.printStackTrace();
		} 
		
		amountOfCalculations--;
		
		if (amountOfCalculations == 0) {
			Long totalTime = times
				    .stream()
				    .mapToLong(Long::longValue)
				    .sum();
			System.out.println("Number of bodies: " + bodies.size());
			System.out.println("Discretized time step for calculation, DT " + DT);
			System.out.println("The number of time steps in the simulation: " + times.size());
			System.out.println("Number of worker threads: " + numberOfWorkers);
			System.out.println("Execution time: " + totalTime * 1e-6 +  " milliseconds");
			
			System.exit(1);
		}
		
	
		
	}

	public List<Body> getBodies() {
		return bodies;
	}
	
	public double getUniverseRadius() {
		return UNIVERSERADIUS;
	}

	public int getNumberOfWorkers() {
		return numberOfWorkers;
	}

	public void setNumberOfWorkers(int numberOfWorkers) {
		this.numberOfWorkers = numberOfWorkers;
	}
	

}
