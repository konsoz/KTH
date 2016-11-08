import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Iterator;
import java.util.stream.Stream;

/**
 * 
 * Pebble Solitaire problem solver
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 *
 */

public class Main {

	private static int count;
	
	public static void play (char game []) {
		
		// Copy the game into original array
		char [] original = new char[game.length];
		for (int i = 0; i < game.length; i++) {
			original[i] = game[i];
		}
		
	    for (int i = 0; i < 11; i++ ) {
	    	// In case there is a "oo"
	        if ( game[i] == 'o' && game[i+1] == 'o') {
	        	// In case there was for example "oo-o", we will move first "o" and delete second
	            if (i + 2 < 12 && game[i+2] == '-' ){
	            	// Change place of "o"
	                game[i + 2] = 'o';
	                game[i] = game[i + 1] = '-';
	                // Try to move more "o"
	                play (game);
	                for (int a = 0; a < game.length; a++) {
	        			game[a] = original[a];
	        		}
	            }
	            // In case there was "--oo--", i.e. two alternatives to solve
	            // "-oo"
	            if ( i - 1 >= 0 && game [i - 1] == '-' ) {
	                game [i - 1] = 'o';
	                game [i] = game [i + 1] = '-';
	                play (game);
	                for (int a = 0; a < game.length; a++) {
	        			game[a] = original[a];
	        		}
	            }
	        }
	    }
	 
	    int c = 0;
	 
	    // Count amount of "o"
	    
	    for ( int i = 0; i < 12; i++ ) {
	        if (game[i] == 'o') c++;
	    }
	   
	    // Keep track on lowest amount of pebbles
	    
	    if ( c < count )
	        count = c;
	}

	public static void main(String[] args) {
		// Read the input
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		Stream<String> stream = in.lines();
		Iterator<String> iter = stream.iterator();
		// Discard number of games
		iter.next();
		
		while(iter.hasNext()) {
			// Take out first game
			String game = iter.next();
			// Max amount is 12
			count = 13; 
			// Play!
			play(game.toCharArray());
			System.out.println(count);
		}
		
		

	}

}
