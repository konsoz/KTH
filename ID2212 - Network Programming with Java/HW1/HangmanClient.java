package se.kth.id2212.hw1;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import java.awt.*;

/**
 * Gui for the Hangman client. Used to submit connect to the Hangman server to
 * play a game of Hangman.
 * 
 * 
 * @author Daniel Buchberger
 * @author Konstantin Sozinov
 * 
 */
@SuppressWarnings("serial")
public class HangmanClient extends JPanel {
    private JButton connectButton;
    private ServerConnection connection;

    private JButton startButton;
    private JLabel scoreLabel;
    private JLabel wordLabel;
    private JLabel failsLabel;
    private JTextField guessField;
    private JButton guessButton; 
    private JLabel guessesLabel;
    
    private JLabel hangmanImage;
    
    /**
     * Creates a new instance and builds the gui.
     */
    HangmanClient() {
        buildGui();
    }

    /**
     * The main method of the client. Starts the gui.
     *
     * @param args No command line parameters are used.
     */
    public static void main(String[] args) {
        JFrame frame = new JFrame("Hangman Client");
        frame.setContentPane(new HangmanClient());
        frame.pack();
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);
    }

	/**
	 * Builds the gui with the different panel components defined below.
	 */
    private void buildGui() {
        setLayout(new BorderLayout());
        add(createConnectPanel(), BorderLayout.NORTH);
        add(createGamePanel(), BorderLayout.CENTER);
        add(createDrawingPanel(), BorderLayout.SOUTH);
    }

    /**
     * 
     * This method will create connect panel. It has one listener on the button.
     * When a user pushes the button, the client will connect to the server with ip and port.
     * 
     * @return connect component
     */
    private Component createConnectPanel() {
    	
        JPanel connectPanel = new JPanel();
        connectPanel.setBorder(
                new TitledBorder(new EtchedBorder(), "Connection"));

        connectPanel.add(new JLabel("Host:"));
        final JTextField hostField = new JTextField("localhost");
        connectPanel.add(hostField);

        connectPanel.add(new JLabel("Port:"));
        final JTextField portField = new JTextField("4444");
        connectPanel.add(portField);

        connectButton = new JButton("Connect");
        connectPanel.add(connectButton);
        
		connectButton.addActionListener(e -> {
			String host = hostField.getText();
			int port = Integer.parseInt(portField.getText());
			connectButton.setEnabled(false);
			connection = new ServerConnection(HangmanClient.this, host, port);
			new Thread(connection).start();
		});
        return connectPanel;
    }
    
	/**
	 * Creates a game panel component with start button, score label, current
	 * guess progress label (_ _ _ _), remaining attempts label, guess field and
	 * guess button.
	 * 
	 * A server call is made when a user pushes a guess button.
	 * 
	 * @return The created game JPanel
	 */
    private Component createGamePanel() {
    	
    	JPanel gamePanel = new JPanel();
    	gamePanel.setBorder(new TitledBorder(new EtchedBorder(), "Game"));
    	gamePanel.setLayout(new GridLayout(4,2));
    	
    	startButton = new JButton("Start game");
    	startButton.setEnabled(false);
    	gamePanel.add(startButton);
    	startButton.addActionListener(e -> {
    		connection.sendMessage("start_game");
        });
    	
    	scoreLabel = new JLabel("Score: 0");
    	gamePanel.add(scoreLabel);
    	
    	wordLabel = new JLabel();
    	gamePanel.add(wordLabel);
    	
    	failsLabel = new JLabel(); //Remaining attempts
    	gamePanel.add(failsLabel);
    	
    	guessField = new JTextField("Your guess!");
    	guessField.setEnabled(false);
    	gamePanel.add(guessField);
    	
    	guessButton = new JButton("Guess");
    	guessButton.setEnabled(false);
    	gamePanel.add(guessButton);
    	guessButton.addActionListener(e -> {
    		connection.sendMessage(guessField.getText());
        });
    	
    	guessesLabel = new JLabel();
    	gamePanel.add(guessesLabel);
    	
    	return gamePanel;
    }
    
	/**
	 * Creates a drawing panel component for drawing the current progress in the
	 * game. It only contains an image within a label.
	 * 
	 * @return
	 */
   private Component createDrawingPanel() {
    	JPanel drawing = new JPanel();
    	drawing.setBorder(new TitledBorder(new EtchedBorder(), "Progress"));
    	hangmanImage = new JLabel(new ImageIcon("hangman/step11.png"));
    	drawing.add(hangmanImage);
    	
    	return drawing;
    }
   
   
   /**
    *  ---- Callback methods to update the gui ----
    */
   
   /**
    * Callback method for the network layer. Should be invoked when
    * successfully connected to the server.
    */
	protected void connected() {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				startButton.setEnabled(true);
			}
		});
	}

	/**
	 * Callback method for the network layer. Should be invoked when
	 * successfully started a game.
	 */
	protected void gameStarted() {
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				guessButton.setEnabled(true);
				guessField.setEnabled(true);
				guessField.setText("");
				startButton.setEnabled(false);
				hangmanImage.setIcon(new ImageIcon("hangman/step11.png"));
			}
		});
	}

	/**
	 * Callback method for the network layer. Should be invoked when the game
	 * properties are to be updated.
	 * 
	 * @param word
	 *            The word to update to.
	 * @param score
	 *            The score to update to.
	 * @param fails
	 *            The number of fails to update to.
	 */
	void updateGame(String word, String score, String fails) {
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				wordLabel.setText(word);
				scoreLabel.setText("Score: " + score);
				failsLabel.setText("Fails: " + fails);
			}
		});
	}
	
	/**
	 * Callback method for the network layer. Should be invoked when a guess
	 * failed.
	 * 
	 * @param guess
	 *            The failed guess.
	 */
	protected void appendGuess(String guess) {
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				if (guessesLabel.getText() == "")
					guessesLabel.setText(guess);
				else
					guessesLabel.setText(guessesLabel.getText() + ", " + guess);
			}
		});
	}
	
    /**
     * Callback method for the network layer. Should be invoked when
     * a guess was incorrect.
     * 
     * @param fails The number of allowed fails left. 
     */
	protected void changeImage(String fails) {
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				hangmanImage.setIcon(new ImageIcon("hangman/step" + fails + ".png"));
			}
		});
    }

	/**
	 * Callback method for the network layer. Should be invoked when
	 * successfully ended a game.
	 */
	protected void gameEnded() {
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				guessButton.setEnabled(false);
				guessField.setEnabled(false);
				startButton.setEnabled(true);
			}
		});
	}

	/**
	 * Callback method for the network layer. Should be invoked when a game is
	 * won.
	 */
	protected void showCongratulation() {
		JOptionPane.showMessageDialog(this,
			    "You have won a game! \nPress \"Start game\" to play another!",
			    "Congratulations!",
			    JOptionPane.PLAIN_MESSAGE);
	}

	/**
	 * Callback method for the network layer. Should be invoked when
	 * a game is lost.
	 */
	protected void showLoss() {
		JOptionPane.showMessageDialog(this,
			    "You lost the game. \nPress \"Start game\" to try again!",
			    "Congratulations!",
			    JOptionPane.PLAIN_MESSAGE);
	}
}
