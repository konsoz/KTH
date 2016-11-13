package se.kth.ID1020.ConnectedGraph;

import edu.princeton.cs.algs4.Queue;
import edu.princeton.cs.algs4.Stack;
import edu.princeton.cs.introcs.StdOut;
import se.kth.id1020.Edge;
import se.kth.id1020.Graph;
import se.kth.id1020.Vertex;

public class BreadthFirstPaths {
	private static final int INFINITY = Integer.MAX_VALUE;
	private boolean[] marked; // marked[v] = is there an s-v path
	private int[] edgeTo; // edgeTo[v] = previous edge on shortest s-v path
	private int[] distTo; // distTo[v] = number of edges shortest s-v path

	/**
	 * Computes the shortest path between the source vertex <tt>s</tt> and every
	 * other vertex in the graph <tt>G</tt>.
	 * 
	 * @param G
	 *            the graph
	 * @param s
	 *            the source vertex
	 */
	public BreadthFirstPaths(Graph G, int s) {
		marked = new boolean[G.numberOfVertices()];
		distTo = new int[G.numberOfVertices()];
		edgeTo = new int[G.numberOfVertices()];
		bfs(G, s);
	}

	// breadth-first search from a single source
	private void bfs(Graph G, int s) {
		Queue<Integer> q = new Queue<Integer>();
		for (int v = 0; v < G.numberOfVertices(); v++)
			distTo[v] = INFINITY;
		distTo[s] = 0;
		marked[s] = true;
		q.enqueue(s);

		while (!q.isEmpty()) {
			int v = q.dequeue();
			Iterable<Edge> it = G.adj(v);
			for (Edge edge : G.adj(v)) {
				if (!marked[edge.to]) {
					edgeTo[edge.to] = v;
					distTo[edge.to] = distTo[v] + 1;
					marked[edge.to] = true;
					q.enqueue(edge.to);
				}
			}
		}
	}

	/**
	 * Is there a path between the source vertex <tt>s</tt> (or sources) and
	 * vertex <tt>v</tt>?
	 * 
	 * @param v
	 *            the vertex
	 * @return <tt>true</tt> if there is a path, and <tt>false</tt> otherwise
	 */
	public boolean hasPathTo(int v) {
		return marked[v];
	}

	/**
	 * Returns the number of edges in a shortest path between the source vertex
	 * <tt>s</tt> (or sources) and vertex <tt>v</tt>?
	 * 
	 * @param v
	 *            the vertex
	 * @return the number of edges in a shortest path
	 */
	public int distTo(int v) {
		return distTo[v];
	}

	/**
	 * Returns a shortest path between the source vertex <tt>s</tt> (or sources)
	 * and <tt>v</tt>, or <tt>null</tt> if no such path.
	 * 
	 * @param v
	 *            the vertex
	 * @return the sequence of vertices on a shortest path, as an Iterable
	 */
	public Iterable<Integer> pathTo(int v) {
		if (!hasPathTo(v))
			return null;
		Stack<Integer> path = new Stack<Integer>();
		int x;
		for (x = v; distTo[x] != 0; x = edgeTo[x])
			path.push(x);
		path.push(x);
		return path;
	}

	public static int getVertex(Graph G) {
		String vertexRenyn = "Renyn"; // 1006
		String nodeParses = "Parses"; // 918

		Iterable<Vertex> it = G.vertices();
		for (Vertex vertex : it) {
			if (vertex.label.compareTo(vertexRenyn) == 0)
				return vertex.id ;
			if (vertex.label.compareTo(nodeParses) == 0)
				 return vertex.id;
		}
		return 0;
	}
}
