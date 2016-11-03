package barens.seq.simulation;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Polygon;
import java.awt.geom.AffineTransform;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSlider;

/**
 * N-Body simulation Barnes-Hut parallel solution. It uses a barrier to
 * synchronize between calculating forces and moving the bodies. Barrier also
 * used as a synchronization between GUI and simulation.
 * 
 * Main class. Acts like a GUI. It will initialize bodies, calculate forces,
 * move bodies and repaint the GUI. It will also take time for one simulation.
 * 
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 */
public class GUI extends JFrame {
	
	private static final long serialVersionUID = 1L;
	
	private Simulation simulation;
	
	private boolean simulate;
	private JSlider slider;
	private JCheckBox showDistance;
	private JCheckBox showLines;
	private JCheckBox showVelocity;
	
	public GUI(Simulation simulation) {
		super("N-body Simulation");
		this.simulation = simulation;
		
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		buildContents();
		pack();
		setVisible(true);
	}

	/**
	 * Build frame content
	 */
	private void buildContents() {
		Container pane = getContentPane();

		PaintFrame pf = new PaintFrame(simulation.getUniverseRadius(),500, simulation);
		pane.add(pf, BorderLayout.SOUTH);
		
		JPanel options = new JPanel(new BorderLayout());
		
		JButton bStart = new JButton("Start");
		options.add(bStart, BorderLayout.WEST);
		bStart.addActionListener(e -> {
			simulate = true;
		});
		
		JPanel sliderPanel = new JPanel(new BorderLayout());
		
		slider = new JSlider(JSlider.HORIZONTAL, 0, 50, 0);

	    slider.setMinorTickSpacing(2);
	    slider.setMajorTickSpacing(10);
	    slider.setPaintTicks(true);
	    slider.setPaintLabels(true);

	    slider.setLabelTable(slider.createStandardLabels(10));

		sliderPanel.add(new JLabel("Time units"), BorderLayout.NORTH);
		sliderPanel.add(slider, BorderLayout.SOUTH);
		options.add(sliderPanel, BorderLayout.CENTER);

		JPanel checkboxes = new JPanel(new GridLayout(2, 2));
		showDistance = new JCheckBox("Show distance to universe center");
		checkboxes.add(showDistance);
		showLines = new JCheckBox("Show lines from center to bodies");
		checkboxes.add(showLines);
		showVelocity = new JCheckBox("Show scaled velocity vector");
		checkboxes.add(showVelocity);
		options.add(checkboxes, BorderLayout.SOUTH);
	
		pane.add(options, BorderLayout.CENTER);

		JButton bStop = new JButton("Stop");
		options.add(bStop, BorderLayout.EAST);
		bStop.addActionListener(e -> {
			simulate = false;
		});
	}
	
	private class PaintFrame extends JPanel {

		private static final long serialVersionUID = 1L;
		
		private Simulation sim;
		private int universeRadius;
		
		private double scale;

		/**
		 * Initialize frame with universe radius and size of the window.
		 * 
		 * @param universeRadius the radius of the simulated universe.
		 * @param drawSize size of the window to draw the universe in.
		 * @param sim the simulation object that simulates the bodies.
		 */
		public PaintFrame(double universeRadius, int drawSize, Simulation sim) {
			setPreferredSize(new Dimension(drawSize, drawSize));
			
			scale = drawSize/(universeRadius*2);
			this.universeRadius = (int) Math.round(universeRadius*scale);
			this.sim = sim;
		}

		/**
		 * Simulate N-Body and repaint the gui with new positions of the bodies.
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
				g.drawOval(x-1, y-1, 2, 2);
				
				if (showVelocity.isSelected()) {
					int vx = (int) (x + (b.getVelocityX()/1E3));
					int vy = (int) (y + (b.getVelocityY()/1E3));
					g.drawLine(x, y, vx, vy);
					
					AffineTransform tx = new AffineTransform();
					
					Polygon arrowHead = new Polygon();
					arrowHead.addPoint(0, 3);
					arrowHead.addPoint(-3, -3);
					arrowHead.addPoint(3, -3);
					
					tx.setToIdentity();
					double angle = Math.atan2(vy-y, vx-x);
					tx.translate(vx, vy);
					tx.rotate((angle-Math.PI/2d));  
					
					Graphics2D g2d = (Graphics2D) g.create();
					g2d.setTransform(tx);   
					g2d.fill(arrowHead);
					g2d.dispose();
				}
				if (showDistance.isSelected()) {
					int distance = (int)Math.round(Math.abs(Math.sqrt(Math.pow(y-universeRadius, 2)+Math.pow(x-universeRadius, 2))));
					if (distance < universeRadius) 
						g.drawString(distance + "", x+2, y-2);
					else {
						double vx = x - universeRadius;
						double vy = y - universeRadius;
						double nx = vx / distance;
						double ny = vy / distance;
						double kx = nx * (universeRadius - universeRadius / 15);
						double ky = ny * (universeRadius - universeRadius / 15);
						int cx = (int) (kx + universeRadius);
						int cy = (int) (ky + universeRadius);
						
						g.drawString(distance + "", cx+2, cy-2);
					}
				}
				if (showLines.isSelected())
					g.drawLine(universeRadius, universeRadius, x, y);
			}
			try {
				Thread.sleep(slider.getValue());
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			if (simulate)
				sim.simulate();

			repaint();
		}		
	}
	
	public static void main(String[] args) {
		int gnumBodies = Integer.parseInt(JOptionPane.showInputDialog(null,"Enter amount of bodies")) - 1;
		int numSteps = Integer.parseInt(JOptionPane.showInputDialog(null,"Number of time steps in the simulation: "));
		
		Simulation sim = new Simulation(gnumBodies, numSteps);
		new GUI(sim);
	}
	
}
