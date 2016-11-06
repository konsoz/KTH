package se.kth.id2212.hw1;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.stream.Stream;



enum State {
	WAITING, PLAYING;
}

/*
 * 
 * This class represents "state" of the server. I.e. it represents state of the game
 * 
 * @author Konstantin Sozinov
 * @author Daniel Buchberger
 * 
 */

public class GameHandler {
	private static final int ALLOWED_FAILS = 11;
	
	State state;
	private String correctWord; // A B C D
	private char[] currentWord; // _ _ _ _
	private int score;
	private int currentFails = ALLOWED_FAILS;
	
	public GameHandler() {
		state = State.WAITING;
	}
	
	/**
	 * 
	 * This method will read words from a file and generate a random word
	 * 
	 */
	
	private void generateNewGame() {
		// Read random line from a file
		List<String> words = new ArrayList<String>();
        Path path = Paths.get("words.txt");
        try (Stream<String> lines = Files.lines(path)) {        	
            lines.forEach(s -> words.add(s));
        } catch (IOException ex) {
        	ex.printStackTrace();
        	System.out.println(ex.getMessage());
        }
        
        // Generate a random number in order to choose a 
        // word from the list.
        
        Random r = new Random();
        int index = r.nextInt(words.size());
        correctWord = words.get(index);
        currentWord = new char[correctWord.length()];
        
        // Initialize underscore template for the word
        for (int i = 0; i < currentWord.length; i++) 
			currentWord[i] = '-';
		
        // Resets allowed fails
        currentFails = ALLOWED_FAILS;
	}
	
	/**
	 * 
	 * This method will check the guess from the user.
	 * A guess can be the whole word or a character
	 * 
	 * @param current guess
	 * @return true if something matched, false otherwise
	 */
	
	private boolean checkGuess(String guess) {
		char character = guess.charAt(0);
		boolean matched = false;
		
		for (int i = 0; i < correctWord.length(); i++) {
			if (Character.toLowerCase(correctWord.charAt(i)) == Character.toLowerCase(character)) {
				currentWord[i] = correctWord.charAt(i);
				matched = true;
			}	
		}
		
		return matched;
	}
	
	/**
	 * 
	 * This method will process the input from the user (guess)
	 * and create the response depending on the input
	 * 
	 * @param input from the user
	 * @return output to the user
	 */
	
	public String processInput(String input) {
		switch (state) {
		case WAITING:
			if (input.equals("start_game")) {
				generateNewGame();
				state = State.PLAYING;
				return "NEWGAME|" + status();
			}
			break;
		case PLAYING:
			if (input.equalsIgnoreCase(correctWord)) { 
				state = State.WAITING;
				score++;
				currentWord = correctWord.toCharArray();
				return "WIN|" + status();
			} else if (input.length() == 1 && checkGuess(input)) {
				if (new String(currentWord).equals(correctWord)) {
					state = State.WAITING;
					score++;
					return "WIN|" + status();
				} else
					return "STATUS|" + status();
			} else {
				currentFails--;
				if (currentFails == 0) {
					state = State.WAITING;
					score--;
					currentWord = correctWord.toCharArray();
					return "LOSS|" + status();
				} else 
					return "FAIL|" + status();
			}
		}
		return null;
	}
	
	/**
	 * 
	 * Status string to the user. Includes state of the word,
	 * score, current fails.
	 * 
	 * Correct word for the debug.
	 * 
	 * @return status string
	 */
	
	private String status() {
		return new String(currentWord) + "|" + score + "|" + currentFails + "|cheat:" + correctWord;
	}
}
