/**
 * 
 * 
 * This class represents a honeybee thread.
 * 
 * 
 * @author konstantin
 *
 */
public class HoneyBee implements Runnable {
	private final int id;
	private final HoneyPot pot;
	private static int SLEEPTIME = 2000;
	
	public HoneyBee(int id, HoneyPot pot){
		this.id = id;
		this.pot = pot;
	}

	public int getId() {
		return id;
	}

	@Override
	public void run() {
		while(true) {
			try {
				
				
				pot.put(this);
				
				Thread.sleep(SLEEPTIME);
				
			} catch (InterruptedException e) {
				System.out.println(e.toString());
			}
		}
	}
	
	
}
