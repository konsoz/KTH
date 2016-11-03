package barens.parallel.simulation;

import java.util.List;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;

/**
 * Defines a worker that handles a number of bodies and how forces act on them.
 */
public class Worker extends Thread {

	private int workerID;
	private int numberOfThreads;
	private Quad universeBounderies;
	private BHTree tree;
	private List<Body> bodies;
	private CyclicBarrier barrier;
	private CyclicBarrier done;

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
	public Worker(int workerID, int numberOfThreads, Quad universeBounderies, BHTree tree, List<Body> bodies,
			CyclicBarrier barrier, CyclicBarrier done) {
		this.workerID = workerID;
		this.numberOfThreads = numberOfThreads;
		this.tree = tree;
		this.barrier = barrier;
		this.bodies = bodies;
		this.done = done;
		this.universeBounderies = universeBounderies;
	}

	/**
	 * Start the worker thread.
	 */
	@Override
	public void run() {
		try {
			done.await(); // ready to start
			calculateForces();
			barrier.await();
			moveBodies();
			done.await(); // done
		} catch (BrokenBarrierException | InterruptedException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Calculate the forces acting on the bodies this worker is responsible for.
	 */
	private void calculateForces() {
		for (int i = workerID; i < bodies.size(); i += numberOfThreads) {
			Body b = bodies.get(i);
			b.resetForces();
			if (universeBounderies.contains(b))
				tree.updateForce(b);
		}
	}

	/**
	 * Moves all bodies this worker is responsible for, according to the
	 * previously computed forces
	 */
	private void moveBodies() {
		for (int i = workerID; i < bodies.size(); i += numberOfThreads) {
			Body b = bodies.get(i);
			if (universeBounderies.contains(b))
				b.update();
		}
	}

}
