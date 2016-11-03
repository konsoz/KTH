package brute.seq.simulation;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.util.Collection;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
/**
 * 
 * N-Body simulation Brute force sequential solution.
 * Main class. Acts like a GUI. It will initialize bodies, calculate forces, move bodies and repaint the GUI.
 * 
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 *
 */
public class GUI extends JFrame {
	
	private Simulation simulation;
	
	public GUI(Simulation simulation) {
		super("N-body Simulation");
		this.simulation = simulation;
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		Container pane = getContentPane();
		PaintFrame pf = new PaintFrame(simulation.getUniverseRadius(),500, simulation);
		pane.add(pf, BorderLayout.CENTER);
		pack();
		setVisible(true);
	}
	
	public static void main(String[] args) {
		int gnumBodies = Integer.parseInt(JOptionPane.showInputDialog(null,"Enter amount of bodies")) - 1;
		int numSteps = Integer.parseInt(JOptionPane.showInputDialog(null,"Number of time steps in the simulation: "));
		Simulation sim = new Simulation(numSteps);
		sim.initBodies(gnumBodies);
		new GUI(sim);
	}
	
	private class PaintFrame extends JPanel {
		
		private Simulation sim;
		private int universeRadius;
		
		private double scale;
		
		/**
		 * 
		 * Initialize frame with universe radius and size of the window.
		 * 
		 * @param universeRadius
		 * @param drawSize
		 * @param sim
		 */
		public PaintFrame(double universeRadius, int drawSize, Simulation sim) {
			setPreferredSize(new Dimension(drawSize, drawSize));
			scale = drawSize/(universeRadius*2);
			this.universeRadius = (int) Math.round(universeRadius*scale);
			this.sim = sim;
		}

		/**
		 * 
		 * Simulate N-Body and repaint the gui with new positions of the bodies.
		 * 
		 */
		@Override
		public void paint(Graphics g) {
			g.setColor(Color.BLACK);
			g.fillRect(0, 0, universeRadius*2, universeRadius*2);
			g.setColor(Color.WHITE);
			g.drawOval(0, 0, universeRadius*2, universeRadius*2);
			for(Body b : sim.getBodies()) {
				g.setColor(b.getColor());
				int x = (int) Math.round(
						b.getPositionX()*scale+universeRadius);
				int y = (int) Math.round(
						b.getPositionY()*scale+universeRadius);
				g.drawOval(x, y, 2, 2);
			}
			simulation.simulate();
			repaint();
		}
	}
	
}
