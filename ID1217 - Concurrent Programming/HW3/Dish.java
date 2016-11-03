/**
 * 
 * Shared object aka monitor. 
 * Represents a dish with some amount of worms.
 * Parent bird will execute refill method.
 * Baby birds will execute getWorm method.
 * 
 * @author konstantin
 *
 */
public class Dish {
	
	private final int maxWorms;
	private int currentWorms;
	private boolean empty;
	
	public Dish(int maxWorms){
		this.maxWorms = maxWorms;
		empty = false;
		currentWorms = maxWorms;
	}
	
	/**
	 * 
	 * This method is used when one baby bird tries to grab a worm.
	 * If dish is empty one thread will wait until the parent bird refills the dish.
	 *  
	 * 
	 * @return a worm (simply an integer)
	 */
	public synchronized int getWorm(BabyBird baby){
        while (currentWorms == 0) {
            try {
				wait();
			} catch (InterruptedException e) {
				System.err.println(e.toString());
			}
        }
		int worm = currentWorms;
		System.out.println("Bird # "+ baby.getId() + "- Mmmm, that's a tasty worm (" + worm + ")!");
		currentWorms--;
		if (currentWorms == 0) {
			System.out.println("Bird # " + baby.getId() + " - The bowl is empty. Hold on guys! -- CHIIIIIRP!!");
			empty = true;
		}
		notify();
		return worm;
    }
	
	/*
	 *
	 * This method is executed by parent bird.
	 * Parent bird thread will wait until condition is true (there are no more worms in the dish)
	 * and then refill the dish.
	 * 
	 */
	public synchronized void refill(){
			try {
				while(!isEmpty()) wait();
				System.out.println();
				System.out.println("Tweet! My babies need food!");
				currentWorms = maxWorms;
				empty = false;
				System.out.println("Tweet! I found " + maxWorms + " worms!");
				System.out.println("Tweet! Now I'm so tired. Zzzz...\n");
				notify();
			} catch (InterruptedException e) {
				System.out.println(e.getMessage());
			}
	}

	public int getCurrentWorms() {
		return currentWorms;
	}

	public synchronized boolean isEmpty(){
		return empty;
	}

	public void setCurrentWorms(int currentWorms) {
		this.currentWorms = currentWorms;
	}


	public int getMaxWorms() {
		return maxWorms;
	}
}
