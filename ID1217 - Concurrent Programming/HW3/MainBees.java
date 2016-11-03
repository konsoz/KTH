/**
 * 
 * 
 * This program illustrates bear and honeybees problem.
 * Given are N(args[1]) honeybees and a hungry bear. They share a pot of honey. 
 * The pot is initially empty; 
 * its capacity is H(args[0]) portions of honey. 
 * The bear sleeps until the pot is full, then eats all the 
 * honey and goes back to sleep. 
 * Each bee repeatedly gathers one portion of honey and puts it in the pot; the bee who fills the pot awakens the bear.
 * The synchronization is done by a monitor object(the honey pot).
 * 
 * @author konstantin
 *
 */
public class MainBees {

	public static void main(String[] args) {
		HoneyPot pot = new HoneyPot(20);
		
		new Thread(new Bear(pot)).start();

		int bees = 4;//Integer.parseInt(4);
		
		for (int i = 0; i < bees; i++) {
			 (new Thread(new HoneyBee(i,pot))).start();
		}
		
	}

}
