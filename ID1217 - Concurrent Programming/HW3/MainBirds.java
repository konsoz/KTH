/**
 * 
 * This program illustrates the hungry birds problem.
 * Given are N(args[1]) baby birds and one parent bird. 
 * The baby birds eat out of a common dish that initially contains W(args[0]) worms. 
 * Each baby bird repeatedly takes a worm, eats it, sleeps for a while, takes another worm, and so on. 
 * If the dish is empty, the baby bird who discovers the empty dish chirps real loud to awaken the parent bird. 
 * The parent bird flies off and gathers W more worms, puts them in the dish, and then waits for the dish to be empty again. 
 * This pattern repeats forever.
 * The synchronization is done by a monitor object.
 * 
 * @author konstantin
 *
 */

public class MainBirds {

	public static void main(String[] args) {
		
		Dish dish = new Dish(Integer.parseInt(args[0]));
		
		new Thread(new ParentBird(dish)).start();	

		int amountBabyBirds = Integer.parseInt(args[1]);
		
		for (int i = 0; i < amountBabyBirds; i++) {
			 (new Thread(new BabyBird(i,dish))).start();
		}
		
	}

}
