package se.kth.ID1020.ConnectedGraph;

import se.kth.id1020.Edge;
import se.kth.id1020.Graph;
import se.kth.id1020.Vertex;
import edu.princeton.cs.algs4.IndexMinPQ;
import edu.princeton.cs.algs4.Stack;

public class DijkstraSP {
    private double[] distTo;          // distTo[v] = distance  of shortest s->v path
    private Edge[] edgeTo;    // edgeTo[v] = last edge on shortest s->v path
    private IndexMinPQ<Double> pq;    // priority queue of vertices

    
    public DijkstraSP(Graph G, int s) {
 
        distTo = new double[G.numberOfVertices()];
        edgeTo = new Edge[G.numberOfVertices()];
        for (int v = 0; v < G.numberOfVertices(); v++)
            distTo[v] = Double.POSITIVE_INFINITY;
        distTo[s] = 0.0;

        // relax vertices in order of distance from s
        
        pq = new IndexMinPQ<Double>(G.numberOfVertices());
        pq.insert(s, distTo[s]);
        while (!pq.isEmpty()) {
            int v = pq.delMin();
            for (Edge e : G.adj(v))
                relax(e);
        }
    }

   
    /**
     * When we consider the a new edge, does it a give the new shortest path?
     * If it does not ignore, it if it does we update the data structure.
     * @param e An edge
     */
    
    private void relax(Edge e) {
        int v = e.from, w = e.to;
        if (distTo[w] > distTo[v] + e.weight) {
            distTo[w] = distTo[v] + e.weight;
            edgeTo[w] = e;
            if (pq.contains(w)) pq.decreaseKey(w, distTo[w]);  //Take the next closest vertex to the source
            else                pq.insert(w, distTo[w]);
        }
    }

    /**
     * Is there a path from the source vertex s to vertex v?
     * @param v the destination vertex
     * @return true if there is a path from the source vertex
     *    s to vertex v, and false otherwise
     */
    public boolean hasPathTo(int v) {
        return distTo[v] < Double.POSITIVE_INFINITY;
    }

    /**
     * Returns a shortest path from the source vertex s to vertex v.
     * @param v the destination vertex
     * @return a shortest path from the source vertex s to vertex v
     *    as an iterable of edges, and null if no such path
     */
    public Iterable<Edge> pathTo(int v) {
        if (!hasPathTo(v)) return null;
        Stack<Edge> path = new Stack<Edge>();
        for (Edge e = edgeTo[v]; e != null; e = edgeTo[e.from]) {
            path.push(e);
        }
        return path;
    }
    
    /**
     * Get a vertex by the given name
     * @param G A graph where the vertex can be found
     * @param source name of a vertex
     * @return vertex id
     */
 
    public static int getVertex(Graph G, String source) {
		Iterable<Vertex> it = G.vertices();
		for (Vertex vertex : it) {
			if (vertex.label.compareTo(source) == 0)
				return vertex.id ;
		}
		return 0;
	}
}
