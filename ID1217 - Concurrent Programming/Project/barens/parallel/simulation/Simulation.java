package barens.parallel.simulation;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;

/**
 * Simulation class, it will start worker threads and distribute the work among them.
 * 
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 */
public class Simulation {
	private final List<Body> bodies;
	private static double SOLARMASS = 1.98892e30;
	private static double UNIVERSERADIUS = 1e18;
	private static double GRAVITATION = 6.67e-11;
	private static double DT = 1e11;
	
    private Quad q = new Quad(0,0,2*UNIVERSERADIUS);

    private int numberOfThreads;
    
    int numSteps;
	int counter;
	long executionTime = 0;
	
	public Simulation(int numberOfThreads, int numberOfBodies, int numSteps) {
		bodies = new ArrayList<Body>();
		this.numberOfThreads = numberOfThreads;
		this.numSteps = numSteps;
		counter = numSteps;
		initBodies(numberOfBodies);
	}

	/**
	 * Initializes the bodies position, velocity and mass. 
	 * 
	 * Source: http://physics.princeton.edu/~fpretori/Nbody/intro.htm
	 * 
	 * @param amountOfBodies the number of bodies to initialize.
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
	 * Runs one simulation step of the simulation.
	 */
	public void simulate() {
		addForces();
	}

	/**
	 * Barnes Hut algorithm.
	 */
	public void addForces() {
		// Create the tree
		
		long beforeTree = System.nanoTime();
		BHTree tree = new BHTree(q);
	  
		// Insert the bodies
		for (Body body : bodies) {
			if (q.contains(body)) // ignores bodies outside of the universe
				tree.insert(body);
		}
		
		long afterTree = System.nanoTime() - beforeTree;
		
		// Paralize the rest of the work
		CyclicBarrier barrier = new CyclicBarrier(numberOfThreads);
		CyclicBarrier done = new CyclicBarrier(numberOfThreads + 1);
		
		// Create threads every time we will simulate, this is extremely bad but it is need
		// in order to calculate performance time.
		// Otherwise threads can be created just ones and synchronized with gui with some barrier.
		for (int i = 0; i < numberOfThreads; i++)
			new Worker(i, numberOfThreads, q, tree, bodies, barrier, done).start();
		
		try {
			done.await(); // Wait for thread creation
			long a = System.nanoTime();
			done.await(); // Wait until all are done
			long b = System.nanoTime() - a;
			executionTime += b;
			executionTime += afterTree;
		} catch (BrokenBarrierException | InterruptedException e) {
			e.printStackTrace();
		}
		if (counter-- == 0) {
			System.out.println("Number of bodies: " + bodies.size());
			System.out.println("Discretized time step for calculation, DT " + DT);
			System.out.println("The number of time steps in the simulation: " + numSteps);
			System.out.println("Execution time: " + executionTime * 1e-6 +  " milliseconds");
			System.exit(1);
		}
		
	}
	
	public List<Body> getBodies() {
		return bodies;
	}
	
	public double getUniverseRadius() {
		return UNIVERSERADIUS;
	}

}
