package queens;

import java.io.IOException;
import java.util.Arrays;
import java.util.Scanner;

import jade.core.AID;
import jade.core.Agent;
import jade.core.behaviours.CyclicBehaviour;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.UnreadableException;

public class QueenAgent extends Agent {

	private int myRow;
	private int numberOfRows;

	@Override
	protected void setup() {
		System.out.println("Queen agent " + getAID().getName() + " is ready");

		// Get the arguments, this queens row and the total number of rows.
		Object[] arguments = getArguments();
		if (arguments.length > 0) {
			myRow = (int) arguments[0];
			numberOfRows = (int) arguments[1];
		}

		// Add behaviour receiving messages.
		addBehaviour(new MessageReceiverBehaviour());

		if (myRow == 0) {
			System.out.println(getLocalName() + ": Is first queen. Starting algorithm.");
			int[] rows = new int[numberOfRows];
			placeQueen(rows, 0);
		}

	}

	
	
	private void printQueens(int[] solution) {
		int n = solution.length;
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				if (solution[i] == j) {
					System.out.print("Q ");
				} else {
					System.out.print("* ");
				}
			}
			System.out.println();
		}
		System.out.println();
		System.out.println("Press \"ENTER\" to generate a new solution");
		Scanner scanner = new Scanner(System.in);
		scanner.nextLine();
		notifyPreviousQueen(solution);

	}

	// check if queen at xth row can be placed at i-th column.
	// offset = 1 means that we
	private void placeQueen(int solution[], int offset) {
		for (int c = solution[myRow] + offset; c < solution.length; c++) {
			if (canPlaceQueen(myRow, c, solution)) {
				solution[myRow] = c;
				if (myRow == solution.length - 1) {
					printQueens(solution);
				} else {
					notifyNextQueen(solution);
				}
			} else {
				notifyPreviousQueen(solution);
			}
		}
	}

	// This function will check if queen can be placed (x2,y2), or we can
	// say that Can queen at x2 row is placed at y2 column.
	// for finding the column for x2 row, we will check all the columns for
	// all the rows till x2-1.
	private boolean canPlaceQueen(int r, int c, int row[]) {
		for (int i = 0; i < r; i++) {
			if (row[i] == c || (i - r) == (row[i] - c) || (i - r) == (c - row[i])) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Sends a message to notify the next so it will position itself.
	 */
	private void notifyNextQueen(int[] rows) {
		ACLMessage message = new ACLMessage(ACLMessage.INFORM);
		message.addReceiver(new AID("q" + (myRow + 1), AID.ISLOCALNAME));

		try {
			message.setContentObject(rows);
		} catch (IOException e) {
			e.printStackTrace();
		}

		send(message);
	}

	/**
	 * Sends a message to the previous queen to re-position itself.
	 */
	private void notifyPreviousQueen(int[] rows) {
		ACLMessage message = new ACLMessage(ACLMessage.FAILURE);
		message.addReceiver(new AID("q" + (myRow - 1), AID.ISLOCALNAME));

		try {
			message.setContentObject(rows);
		} catch (IOException e) {
			e.printStackTrace();
		}

		send(message);
	}

	class MessageReceiverBehaviour extends CyclicBehaviour {

		@Override
		public void action() {
			ACLMessage message = receive();

			if (message != null) {
				int[] rows = null;

				try {
					rows = (int[]) message.getContentObject();
				} catch (UnreadableException e) {
					e.printStackTrace();
				}

				switch (message.getPerformative()) {
				case ACLMessage.FAILURE:
					placeQueen(rows, 1);
					break;
				case ACLMessage.INFORM: // Informed of successful placement
					placeQueen(rows, 0);
					break;

				default:
					break;
				}
			} else {
				block();
			}
		}

	}
}
