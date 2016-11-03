/**
 * 
 * This object acts like a monitor for the bear and honeybees.
 * 
 * 
 * @author konstantin
 *
 */
public class HoneyPot {
	private final int capacity;
	private int amountHoney;
	
	public HoneyPot(int capacity){
		this.capacity = capacity;
	}
	
	/**
	 * 
	 * This method is evoked when a bee wants to put some honey into pot.
	 * If pot is full a bee thread will wait until bear eats everything.
	 * 
	 * @param bee - accessing bee thread
	 */
	public synchronized void put(HoneyBee bee){
		while(capacity == amountHoney)
			try {
				System.out.println("Bee # " + bee.getId() + " is waiting for bear");
				wait();
			} catch (InterruptedException e) {
				System.out.println(e.toString());
			}
		System.out.println("Bee # " + bee.getId() + " puted some honey into pot");
		amountHoney++;
		if(amountHoney == capacity) {
			System.out.println("Bee # " + bee.getId() + " awakens the bear ");
			notify();
		}
	}
	/**
	 * 
	 * This method is evoked when bear tries to eat.
	 * If pot is not full the bear will wait.
	 * Otherwise the bear will empty the pot and wake up all bees.
	 * 
	 */
	public synchronized void eat(){
		while(amountHoney < capacity)
			try {
				System.out.println("Bear is sleeping...");
				wait();
			} catch (InterruptedException e) {
				System.out.println(e.toString());
			}
		System.out.println("Bear is hungry and eats!");
		amountHoney = 0;
		System.out.println("There are no more honey... I should go back to sleep again");
		notifyAll();
		
	}
}
