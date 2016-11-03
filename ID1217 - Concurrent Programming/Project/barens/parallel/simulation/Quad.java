package barens.parallel.simulation;

/**
 * Defines a quadrant.
 */
public class Quad {

	private double x, y, sideLength;

	/**
	 * Creates a new quadrant with the given parameters.
	 * 
	 * @param x
	 *            the quadrants midpoints x coordinate.
	 * @param y
	 *            the quadrants midpoints y coordinate.
	 * @param sideLength
	 *            the quadrants sides length.
	 */
	public Quad(double x, double y, double sideLength) {
		this.x = x;
		this.y = y;
		this.sideLength = sideLength;
	}

	/**
	 * Gets the quadrants side length.
	 * 
	 * @return the side length.
	 */
	public double sideLength() {
		return sideLength;
	}

	/**
	 * Tests if the quadrant contains a given point.
	 * 
	 * @param x
	 *            the point x coordinate.
	 * @param y
	 *            the point y coordinate.
	 * @return the <code>true</code> if the quadrant contain the point and
	 *         <code>false</code> if not.
	 */
	public boolean contains(double x, double y) {
		if (x <= this.x + sideLength / 2.0 && 
			x >= this.x - sideLength / 2.0	&& 
			y <= this.y + sideLength / 2.0 && 
			y >= this.y - sideLength / 2.0) 
			return true;
		return false;
	}
	
	/**
	 * Checks if a body is inside of this quadrant.
	 * 
	 * @param b
	 *            the body to check.
	 * @return the <code>true</code> if the quadrant contain the point and
	 *         <code>false</code> if not.
	 */
	public boolean contains(Body b) {
		return contains(b.getPositionX(), b.getPositionY());
	}

	/**
	 * Creates the north west quadrant of this quadrant.
	 * 
	 * @return the new quadrant.
	 */
	public Quad NW() {
		return new Quad(
			x - sideLength / 4.0, 
			y + sideLength / 4.0, 
			sideLength / 2.0);
	}


	/**
	 * Creates the north east quadrant of this quadrant.
	 * 
	 * @return the new quadrant.
	 */
	public Quad NE() {
		return new Quad(
			x + sideLength / 4.0, 
			y + sideLength / 4.0, 
			sideLength / 2.0);
	}

	/**
	 * Creates the south west quadrant of this quadrant.
	 * 
	 * @return the new quadrant.
	 */
	public Quad SW() {
		return new Quad(
			x - sideLength / 4.0, 
			y - sideLength / 4.0, 
			sideLength / 2.0);
	}


	/**
	 * Creates the south east quadrant of this quadrant.
	 * 
	 * @return the new quadrant.
	 */
	public Quad SE() {
		return new Quad(
			x + sideLength / 4.0, 
			y - sideLength / 4.0, 
			sideLength / 2.0);
	}

}
