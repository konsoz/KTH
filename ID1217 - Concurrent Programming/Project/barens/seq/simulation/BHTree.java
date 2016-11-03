package barens.seq.simulation;

import barens.seq.simulation.BHTree;
import barens.seq.simulation.Body;
import barens.seq.simulation.Quad;

/**
 * Defines a Barnes-Hut quadtree that is used in the N-body simulation for
 * approximating the forces acting on the bodies.
 */
public class BHTree {

	/*
	 * Approximation threshold variable: Bigger value makes for a faster, but
	 * less accurate, simulation while smaller values creates a slower but more
	 * accurate simulation.
	 */
	private static final double THETA = 0.5;

	private Body body;
	private Quad quad;
	private BHTree NW;
	private BHTree NE;
	private BHTree SW;
	private BHTree SE;

	/**
	 * Creates a new empty Barner-Hut quadtree.
	 * 
	 * @param q
	 *            the starting quadrant.
	 */
	public BHTree(Quad q) {
		quad = q;
		body = null;
		NW = null;
		NE = null;
		SW = null;
		SE = null;
	}

	/**
	 * Adds the given body to the tree.
	 * 
	 * @param b
	 *            the body to add.
	 */
	public void insert(Body b) {
		/* If the node is empty. Set its body to the new body. */
		if (body == null) {
			body = b;
			return;
		}

		/*
		 * If the node is internal we have to add the body to the combined
		 * "center-of-mass" and place the body in one of the sub quadrants.
		 */
		if (!isExternal()) {
			body = body.add(b);
			putBody(b);
		}
		/*
		 * If the node is external we create the sub nodes representing each sub
		 * quadrant and add both the new body as well as the body that was
		 * previously in this node to whichever quadrants they are located in.
		 * Lastly we replace the previous body with a new "center-of-mass" body
		 * by adding together the new body and the previous one.
		 */
		else {
			NW = new BHTree(quad.NW());
			NE = new BHTree(quad.NE());
			SE = new BHTree(quad.SE());
			SW = new BHTree(quad.SW());

			putBody(body);
			putBody(b);

			body = body.add(b);
		}
	}

	/**
	 * Inserts a body into the correct sub quadrant by checking which one
	 * contains it.
	 * 
	 * @param b
	 *            the body to be inserted.
	 */
	private void putBody(Body b) {
		if (quad.NW().contains(b))
			NW.insert(b);
		else if (quad.NE().contains(b))
			NE.insert(b);
		else if (quad.SE().contains(b))
			SE.insert(b);
		else
			SW.insert(b);
	}

	/**
	 * Test if this node is an external node
	 * 
	 * @return <code>true</code> if all sub nodes are <code>null</code>
	 *         otherwise <code>false</code>.
	 */
	private boolean isExternal() {
		return (NW == null && NE == null && SW == null && SE == null);
	}

	/**
	 * Update the forces acting on a given body.
	 * 
	 * @param b
	 *            the body to update the forces of.
	 */
	public void updateForce(Body b) {
		if (body == null || b.equals(body))
			return;

		// if the current node is external, update net force acting on b
		if (isExternal())
			b.addForce(body);

		// for internal nodes
		else {

			// width of region represented by internal node
			double s = quad.sideLength();

			// distance between Body b and this node's center-of-mass
			double d = body.distanceTo(b);

			// compare ratio (s / d) to threshold value Theta
			if ((s / d) < THETA)
				b.addForce(body); // b is far away

			// recurse on each of current node's children
			else {
				NW.updateForce(b);
				NE.updateForce(b);
				SW.updateForce(b);
				SE.updateForce(b);
			}
		}
	}

}