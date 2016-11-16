package se.kth.ID1020.ConnectedGraph;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;

import edu.princeton.cs.introcs.StdOut;
import se.kth.id1020.Edge;
import se.kth.id1020.Graph;
import se.kth.id1020.DataSource;
import se.kth.id1020.Vertex;

public class Paths {
	public static void main(String[] args) {
		
		/**
		 * Set up the data
		 */
		
		Graph g = DataSource.load();
		int s = DijkstraSP.getVertex(g, "Renyn");
		int dst = DijkstraSP.getVertex(g, "Parses");
		double cost;
		
		/**
		 * Connected components
		 */
		
		CC connectedCompontents = new CC(g);
	
		if (connectedCompontents.count() == 0)
			System.err.println("G is directly connected");
		else
			System.out.println("Conntected components: "
					+ connectedCompontents.count());

		
		/**
		 * Shortest path including edge-weights
		 */
		
		DijkstraSP sp = new DijkstraSP(g, s);
		cost = 0;
		int jump = 0;
		
		if (sp.hasPathTo(dst)) {
			System.out.println("Path from Renyn to Parses including edge-weights: ");
			System.out.println(s + " (Renyn) " + " to " + " (Parses) " + dst);
			System.out.println("Path: ");
			if (sp.hasPathTo(dst)) {
				for (Edge e : sp.pathTo(dst)) {
					jump++;
					cost += e.weight;
					System.out.print(g.vertex(e.to).label + "->");
				}
			}
			System.out.println();
			System.out.println("Cost: " + cost);
			System.out.println("Amount of jumps :" +jump);
		}	
		
		/**
		 * Shortest path ignoring edge-weights
		 */

		DijkstraSPIgnore spIgonore = new DijkstraSPIgnore(g, s);
		
		jump = 0;
		cost = 0;
		if (spIgonore.hasPathTo(dst)) {
			System.out.println("Path from Renyn to Parses ignoring edge-weights: ");
			System.out.println(s + " (Renyn) " + " to " + " (Parses) " + dst);
			System.out.println("Path: ");
			if (spIgonore.hasPathTo(dst)) {
				for (Edge e : spIgonore.pathTo(dst)) {
					cost += e.weight;
					jump++;
					System.out.print(g.vertex(e.to).label + "->");
				}
			}
			System.out.println();
			System.out.println("Cost: " + cost);
			System.out.println("Amount of jumps"+jump);
		}
	}
}

