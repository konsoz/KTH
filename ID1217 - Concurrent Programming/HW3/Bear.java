/**
 * 
 * 
 * This class represents a bear thread
 * 
 * @author konstantin
 *
 */
public class Bear implements Runnable {

	private final HoneyPot pot;
	private static int SLEEPTIME = 2000;
	
	public Bear(HoneyPot pot) {
		this.pot = pot;
	}
	
	@Override
	public void run() {
		while(true) {
			try {
				pot.eat();
				Thread.sleep(SLEEPTIME);
			} catch (InterruptedException e) {
				System.out.println(e.toString());
			}
		}
	}

}
