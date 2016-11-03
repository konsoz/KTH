package brute.parallel.simulation;

import java.util.List;
import java.util.Timer;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.locks.Lock;

/**
 * 
 * Worker thread, for each body i assigned to a worker,
 * the worker computes forces between the body i and the bodies i+1,...,n.
 * Bodies will be assignet using reverse stripes method, i.e. thread number 1
 * will get bodies 1,5,9; thread number 2 will get bodies 2,6,10 and so on.
 * 
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 *
 */
public class Worker extends Thread {

	private static double GRAVITATION = 6.67e-11;
	private static double DT = 1e11;
	
	private int workerID;
	private int numberOfThreads;
	private List<Body> bodies;
	private Vector[][] forces;
	private CyclicBarrier barrier;
	private CyclicBarrier lock;
	
	/**
	 * Create a new worker with given parameters.
	 * 
	 * @param workerID
	 *            the ID of the worker.
	 * @param numberOfThreads
	 *            the number of workers started.
	 * @param universeBounderies
	 *            the quadrant that defines the universe.
	 * @param tree
	 *            the tree containing the bodies and combined bodies.
	 * @param bodies
	 *            all the bodies.
	 * @param barrier
	 *            combined barrier between the workers.
	 * @param done
	 *            combined barrier between the workers and the master thread.
	 */
	public Worker(int workerID, int numberOfThreads, List<Body> bodies, Vector[][] forces, CyclicBarrier barrier, CyclicBarrier lock) {
		this.workerID = workerID;
		this.numberOfThreads = numberOfThreads;
		this.bodies = bodies;
		this.forces = forces;
        this.barrier = barrier;
        this.lock = lock;
	}
	
	/**
	 * Start the worker thread.
	 */
	@Override
	public void run() {
		try {
				lock.await();  // ready to start
				calculateForces();
				barrier.await();
				moveBodies();
				lock.await(); // done
		} catch (BrokenBarrierException | InterruptedException e) {
			e.printStackTrace();
		} 
	}
	
	/**
	 * 
	 * Calculate the forces.
	 * 
	 */
	private void calculateForces() {

		double distance;
		double magnitute;
		
		double directionX;
		double directionY;
		
		// Calculate the forces using reverse stripes method.
		for(int i = workerID; i < bodies.size()-1; i+=numberOfThreads) {
			for(int j = i+1; j < bodies.size(); j++){
				Body iBody = bodies.get(i);
				Body jBody = bodies.get(j);
				double a = Math.pow(iBody.getPositionX() - jBody.getPositionX(),2);
				double b = Math.pow(iBody.getPositionY() - jBody.getPositionY(),2);
				distance = Math.sqrt(a+b);
				magnitute = (GRAVITATION*iBody.getMass()*jBody.getMass()) / Math.pow(distance,2);
				directionX = jBody.getPositionX() - iBody.getPositionX();
				directionY = jBody.getPositionY() - iBody.getPositionY();
				
				forces[workerID][i].setX(forces[workerID][i].getX() + magnitute*directionX/distance);
				forces[workerID][j].setX(forces[workerID][j].getX() - magnitute*directionX/distance);
				forces[workerID][i].setY(forces[workerID][i].getY() + magnitute*directionY/distance);
				forces[workerID][j].setY(forces[workerID][j].getY() - magnitute*directionY/distance);
				 
			}
		}
	}
	
	/**
	 * 
	 * Move the bodies.
	 * 
	 */
	private void moveBodies() {
		Vector force = new Vector(0.0,0.0);
		
		
		for(int i = workerID ; i<bodies.size();i+=numberOfThreads){
			// Sum up all forces from different threads
			for(int k = 0; k < numberOfThreads; k++) {
				force.setX(force.getX()+forces[k][i].getX());
				forces[k][i].setX(0.0);
				force.setY(force.getY()+forces[k][i].getY());
				forces[k][i].setY(0.0);
			}
			
			// Move the body
			Body body = bodies.get(i);
			
			double deltaVx = force.getX()/body.getMass()*DT;
			double deltaVy = force.getY()/body.getMass()*DT;
			double deltaPx = (body.getVelocityX() + deltaVx/2) * DT;
			double deltaPy = (body.getVelocityY() + deltaVy/2) * DT;
			
			body.setVelocityX(body.getVelocityX()+deltaVx);
			body.setVelocityY(body.getVelocityY()+deltaVy);

			body.setPositionX(body.getPositionX() + deltaPx);
			body.setPositionY(body.getPositionY() + deltaPy);
			
			force.setX(0.0);
			force.setY(0.0);
		}
	}

}
