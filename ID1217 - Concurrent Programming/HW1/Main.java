
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

/**
 * 
 * This program finds polindromic words in a standard UNIX dictionary.
 * It uses pool of threads in order to find polindromic words.
 * Each thread takes one word and works on it.
 * 
 * First argument is amount of threads.
 * 
 * @author Konstantin Sozinov
 *
 */
public class Main {

	public static void main(String[] args) {
		// Create master class and read file
		
		long startTime = System.currentTimeMillis();
		Master master = new Master();
		int numberOfThreads = Integer.parseInt(args[0]);
		
		// Create pool of threads
		ExecutorService executor = Executors.newFixedThreadPool(numberOfThreads);
		for (int i = 0; i <= numberOfThreads; i++) {
			Runnable worker = new Worker(master,master.getWords());
			executor.execute(worker);
		}
		
		// Wait for all threads to terminate
		executor.shutdown();
		try {
			executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
		} catch (InterruptedException e) {
			System.out.println(e.getMessage());
		}
		
		
		// Write all polindroms to file.
		List<String> polindroms = master.getPolindroms();
		
		// Write number of polindroms and elapsed time
		System.err.println("Number of polindroms: " + polindroms.size());
		long stopTime = System.currentTimeMillis();
		long elapsedTime = stopTime - startTime;
		System.out.println("Elapsed time : " + elapsedTime + " milliseconds");
		
		try {
			PrintWriter writer = new PrintWriter("polindrom.txt", "UTF-8");
			for (String polindrom : polindroms) {
				writer.write(polindrom + "\n");
			}
			writer.close();
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}
		
		
	}

	
	
	
}
