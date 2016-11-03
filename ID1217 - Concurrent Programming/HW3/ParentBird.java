/**
 * 
 * 
 * This object represent a parent bird.
 * 
 * @author konstantin
 *
 */
public class ParentBird implements Runnable {

	private final Dish dish;
	private static int SLEEPTIME = 2000;
	
	public ParentBird(Dish dish) {
		this.dish = dish;
	}
	
	
	@Override
	public void run() {
		while(true) {
			try {
				dish.refill();
				Thread.sleep(SLEEPTIME);
			} catch (InterruptedException e) {
				System.out.println(e.getMessage());
			}
		}
	}
}
