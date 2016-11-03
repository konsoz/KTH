/**
 * 
 * This object represents a baby bird.
 * 
 * @author konstantin
 *
 */
public class BabyBird implements Runnable {
	
	private final int id;
	private final Dish dish;
	private static int SLEEPTIME = 2000;
	
	public BabyBird(int id, Dish dish){
		this.id = id;
		this.dish = dish;
	}
	
	@Override
	public void run() {
		while(true) {
			try {
				
				// Play
				
				System.out.println("Bird # " + id + " plays around for a bit");
				Thread.sleep(SLEEPTIME);
				
				// Try to grab a worm
				
				System.out.println("Bird # " + id + "- I am starving, I should go and get some food");
				
				dish.getWorm(this);
				
				// Eat
				Thread.sleep(SLEEPTIME);
									
				
			} catch (InterruptedException e) {
				System.out.println(e.getMessage());
			}
		}
	}

	public int getId() {
		return id;
	}
}
