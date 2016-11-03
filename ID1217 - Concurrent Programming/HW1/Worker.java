import java.util.List;


/**
 * 
 * Worker thread, gets a words and finds polindroms in dictionary.
 * Thread terminates when there are no more words in dictionary.
 * 
 * @author Konstantin Sozinov
 *
 */

public class Worker implements Runnable {

	private final Master master;
	private final List<String> words;

	public Worker(Master master, List<String> words) {
		this.master = master;
		this.words = words;
	}

	@Override
	public void run() {
		while(master.hasWords()) {
			String word = master.take();
			if(word != null) {
				StringBuilder str = new StringBuilder(word);
				String reversed = str.reverse().toString();
				for (String w : words) {
					if (reversed.equalsIgnoreCase(w)) {
						master.put(word);
					}
				}
			}
		}
	}
}
