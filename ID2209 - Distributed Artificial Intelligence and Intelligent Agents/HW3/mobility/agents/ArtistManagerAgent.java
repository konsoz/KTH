package mobility.agents;

import mobility.behaviors.AuctioneerBehavior;

import java.util.HashMap;

import jade.content.ContentElement;
import jade.content.lang.sl.SLCodec;
import jade.content.onto.basic.Action;
import jade.content.onto.basic.Result;
import jade.core.AID;
import jade.core.Agent;
import jade.core.Location;
import jade.core.behaviours.CyclicBehaviour;
import jade.core.behaviours.OneShotBehaviour;
import jade.core.behaviours.SequentialBehaviour;
import jade.domain.*;
import jade.domain.JADEAgentManagement.QueryPlatformLocationsAction;
import jade.domain.mobility.MobilityOntology;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import mobility.models.Artifact;
import mobility.models.Auction;

@SuppressWarnings("serial")
public class ArtistManagerAgent extends Agent {

	private static int PRICE = 1500;

	private HashMap<String, Location> locations;
	private Location home;
	private AID originalAgent;
	private HashMap<String, Double> sellPrices;
	private Auction auction;

	@Override
	protected void setup() {
		System.out.println("Artist manager agent " + getAID().getName() + " is ready");
		auction = new Auction(new Artifact(), PRICE);

		// Setup, save original agents AID and container to return to.
		locations = new HashMap<String, Location>();
		sellPrices = new HashMap<String, Double>();
		originalAgent = getAID();
		home = here();

		getContentManager().registerLanguage(new SLCodec());
		getContentManager().registerOntology(MobilityOntology.getInstance());

		getLocations();

		cloneAuctioneer();

		returnToHomeBehaivour();
	}

	private void getLocations() {
		// Get available locations with AMS

		sendRequest(new Action(getAMS(), new QueryPlatformLocationsAction()));

		// Receive response from AMS
		try {
			MessageTemplate mt = MessageTemplate.and(MessageTemplate.MatchSender(getAMS()),
					MessageTemplate.MatchPerformative(ACLMessage.INFORM));
			ACLMessage resp = blockingReceive(mt);
			ContentElement ce = getContentManager().extractContent(resp);
			Result result = (Result) ce;
			jade.util.leap.Iterator it = result.getItems().iterator();

			while (it.hasNext()) {
				Location loc = (Location) it.next();
				locations.put(loc.getName(), loc);
			}
			locations.remove(here().getName());
			locations.remove("Main-Container");
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	private void returnToHomeBehaivour() {
		addBehaviour(new CyclicBehaviour() {

			@Override
			public void action() {
				MessageTemplate mt = MessageTemplate.MatchOntology("END_OF_AUCTION");
				ACLMessage message = receive(mt);

				if (message != null) {
					double sellPrice = Double.parseDouble(message.getContent());
					sellPrices.put(message.getSender().getLocalName(), sellPrice);

					if (sellPrices.size() == locations.size()) {
						double highestPrice = Double.MIN_VALUE;
						for (double price : sellPrices.values()) {
							if (price > highestPrice) {
								highestPrice = price;
							}
						}
						System.out.println("Highest price for auction was: " + highestPrice);
					}
				} else {
					block();
				}
			}
		});
	}

	// Clone agents so there is one agent for each container.
	private void cloneAuctioneer() {
		int cloneNumber = 1;
		SequentialBehaviour cloningBehaviour = new SequentialBehaviour();
		for (final Location location : locations.values()) {
			final String cloneName = getLocalName() + " clone " + cloneNumber;
			System.out.println(cloneName);
			cloningBehaviour.addSubBehaviour(new OneShotBehaviour() {
				@Override
				public void action() {
					if (getLocalName().contains("clone") == false) {
						doClone(location, cloneName);
					}
				}
			});
			cloneNumber++;
		}
		addBehaviour(cloningBehaviour);
	}

	/**
	 * 
	 * Get all avaiable locations from AMS
	 * 
	 * @param action
	 */
	private void sendRequest(Action action) {

		ACLMessage request = new ACLMessage(ACLMessage.REQUEST);
		request.setLanguage(new SLCodec().getName());
		request.setOntology(MobilityOntology.getInstance().getName());
		try {
			getContentManager().fillContent(request, action);
			request.addReceiver(action.getActor());
			send(request);
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	@Override
	protected void afterClone() {
		super.afterClone();
		addBehaviour(new AuctioneerBehavior(ArtistManagerAgent.this, auction) {

			@Override
			public int onEnd() {
				ACLMessage message = new ACLMessage(ACLMessage.INFORM);
				message.setOntology("END_OF_AUCTION");
				message.addReceiver(originalAgent);
				message.setContent(String.valueOf(getSellPrice()));
				send(message);

				doMove(home);

				return super.onEnd();
			}
		});
	}

	/**
	 * Deletes the agent after it moved back to the home container. (After a
	 * while)
	 */
	@Override
	protected void afterMove() {
		super.afterMove();
		doWait(5000);
		doDelete();
	}

}
