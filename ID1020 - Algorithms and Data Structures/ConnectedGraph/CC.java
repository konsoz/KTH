package se.kth.ID1020.ConnectedGraph;

import se.kth.id1020.Edge;
import se.kth.id1020.Graph;


/**
 * Computes amount of connected components in a given undirected graph.
 * 
 */


public class CC {
    private boolean[] marked;   // marked[v] = has vertex v been marked?
    private int[] id;           // id[v] = id of connected component containing v
    private int[] size;         // size[id] = number of vertices in given component
    private int count;          // number of connected components

    /**
     * Computes the connected components of the undirected graph.
     * @param G the graph
     */
    public CC(Graph G) {
        marked = new boolean[G.numberOfVertices()];
        id = new int[G.numberOfVertices()];
        size = new int[G.numberOfVertices()];
        for (int v = 0; v < G.numberOfVertices(); v++) {
            if (!marked[v]) {
                dfs(G, v);
                count++;
            }
        }
    }

    /*
     * Performs deep first search.
     * Mark vertex v as visited, recursivly visit all unmarked vertices adjacent to v.
     */
    
    private void dfs(Graph G, int v) {
        marked[v] = true;
        id[v] = count;
        size[count]++;
    		for (Edge edge : G.adj(v)) {
    			if (!marked[edge.to])
    				dfs(G,edge.to);
        }
    }
    /**
     * Returns the number of connected components.
     * @return the number of connected components
     */
    public int count() {
        return count;
    }
}
