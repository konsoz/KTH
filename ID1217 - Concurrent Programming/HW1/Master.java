import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Stream;


/**
 * 
 * Master class, used to parse file with words and provides access to them
 * with mutual exclusion in order to avoid race condition.
 * 
 * @author Konstantin Sozinov
 *
 */
public class Master {
	private final List<String> polindroms;
	private final List<String> words;
	private final Lock lock;
	private int amountWords;
	private int currentWord;
	private boolean hasWords;
	
	
	public Master() {
		polindroms = new ArrayList<String>();
		words = new ArrayList<String>();
		lock = new ReentrantLock();
		
		Path path = Paths.get("/home/konstantin/Documents/KTH/ID1217/PolindromicWords/words.txt");
		try (Stream<String> lines = Files.lines(path)) {
			lines.forEach(s -> words.add(s));
			
		} catch (IOException ex) {
			System.out.println(ex.getMessage());
		}
		
		amountWords = words.size();
		currentWord = 0;
		hasWords = true;
	}

	/**
	 * 
	 * Returns one word from dictionary for worker thread.
	 * Uses mutual exclusion to avoid threads get same word.
	 * 
	 * @return A word from dictionary.
	 */
	public String take() {
		lock.lock();
		try {
		 if(currentWord == amountWords) {
			 hasWords = false ;
			 return null;
		 }
		 String word = words.get(currentWord);
		 currentWord++;
		 return word;
		} finally {
			lock.unlock();
		}
	}

	/**
	 * 
	 * Used in order to put one polindrom in the list of polindroms.
	 * Uses mutual exclusion to avoid race condition.
	 * 
	 * @param polindrom - palindromic word
	 */
	public void put(String polindrom) {
		lock.lock();
		try {
			polindroms.add(polindrom);
		} finally {
			lock.unlock();
		}
	}
	
	/**
	 * 
	 * Check if there are words left in dictionary.
	 * 
	 * @return - true - there are more words. false - there are no more words.
	 */
	public boolean hasWords(){
		lock.lock();
		try {
			return hasWords;	
		} finally {
		lock.unlock();
		}
	}
	
	public List<String> getPolindroms(){
		return polindroms;
	}

	public List<String> getWords(){
		return words;
	}
	
}
