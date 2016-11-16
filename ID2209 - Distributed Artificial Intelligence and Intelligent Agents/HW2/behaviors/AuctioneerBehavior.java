package behaviors;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import jade.core.AID;
import jade.core.Agent;
import jade.core.behaviours.CyclicBehaviour;
import jade.core.behaviours.ParallelBehaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.lang.acl.UnreadableException;
import models.Auction;
import models.Auction.Status;
import models.ReqOntology;

public class AuctioneerBehavior extends ParallelBehaviour {

	private List<AID> participants;
	private Auction auction;
	private double decreasingAmount;
	private final static double DECREASING_RATIO = 20;
	private final static int ONE_SEC = 1000;

	public AuctioneerBehavior(Agent artistManagerAgent, Auction auction) {
		super(artistManagerAgent, ParallelBehaviour.WHEN_ALL);
		participants = new ArrayList<>();
		this.auction = auction;

		decreasingAmount = auction.getRealPrice() / DECREASING_RATIO;

		findParticipants();

		System.out.println("Auction is aboout to start with following item " + auction.getArtifact()
				+ " for following price " + auction.getCurrentBid());

		informStart();

		addBidBehaivor();

		addReceivingBidsBehaivor();

	}

	private void addReceivingBidsBehaivor() {
		addSubBehaviour(new CyclicBehaviour() {

			@Override
			public void action() {
				// Receive msg about bid
				MessageTemplate msgTemplate = MessageTemplate.MatchOntology(ReqOntology.AUCTION);
				ACLMessage msg = myAgent.receive(msgTemplate);

				if (msg != null) {
					switch (msg.getPerformative()) {

					case ACLMessage.PROPOSE: {
						Auction aucFromBuyer = null;
						
						// Get proposal, aka action object from participant
						try {
							aucFromBuyer = (Auction) msg.getContentObject();
						} catch (UnreadableException e1) {
							e1.printStackTrace();
						}

						// Auction need to be rolling on
						if (auction.getCurrentStatus().equals(Status.ONGOING)) {
							if (aucFromBuyer.getCurrentBid() == auction.getCurrentBid()) {
								System.out.println("Proposal from bidder " + msg.getSender().getLocalName() 
										+ " is accepted " + " item is sold for " + auction.getCurrentBid());
								acceptProposal(msg);
							} else {
								// Someone proposed, but it was to late...
								System.out.println("Proposal from bidder rejected - " + auction.getCurrentBid()
										+ " from " + msg.getSender().getLocalName());
								rejectProposal(msg);	
							}
						} else {
							// Auction ended
							System.out.println("Proposal from bidder " + msg.getSender().getName()
									+ " rejected, auction already ended...");

							rejectProposal(msg);
						}
						break;
					}

					default:
						break;

					}
				} else {
					block();
				}
			}
		});
	}

	private void acceptProposal(ACLMessage msg) {
		auction.setCurrentStatus(Status.FINISHED_SOLD);
		auction.setBuyer(msg.getSender());

		ACLMessage reply = msg.createReply();
		reply.setPerformative(ACLMessage.ACCEPT_PROPOSAL);

		try {
			reply.setContentObject(auction);
		} catch (IOException e) {
			e.printStackTrace();
		}

		reply.setOntology(ReqOntology.AUCTION);
		myAgent.send(reply);
	}
	
	private void rejectProposal(ACLMessage msg){
		ACLMessage reply = msg.createReply();
		reply.setPerformative(ACLMessage.REJECT_PROPOSAL);

		try {
			reply.setContentObject(auction);
		} catch (IOException e) {
			e.printStackTrace();
		}

		reply.setOntology(ReqOntology.AUCTION);
		myAgent.send(reply);
	}
	
	private void addBidBehaivor() {
		addSubBehaviour(new TickerBehaviour(myAgent, ONE_SEC) {

			@Override
			protected void onTick() {

				if (!auction.getCurrentStatus().equals(Status.ONGOING)) {
					System.out.println("End of the auction... " + auction.getCurrentBid());
					informEnd();
					stop();
				}

				double newBid = auction.getCurrentBid() - decreasingAmount;

				// Lower the bid, send to participants
				if (newBid > auction.getMinSellPrice()) {
					System.out.println("Sending new bid to participants: " + newBid);
					auction.setCurrentBid(newBid);
					informNewBid();
				} else {
					System.out.println("Auction ended without selling, last bid: " + auction.getCurrentBid());
					auction.setCurrentStatus(Status.FINISHED_NOT_SOLD);
					informEnd();
				}
			}
		});
	}

	/**
	 * 
	 * Inform all participants about start
	 * 
	 */
	private void informStart() {

		auction.setCurrentStatus(Status.ONGOING);

		ACLMessage message = new ACLMessage(ACLMessage.INFORM);

		for (AID participant : participants) {
			message.addReceiver(participant);
		}

		try {
			message.setContentObject(auction);
		} catch (IOException e) {
			e.printStackTrace();
		}

		message.setOntology(ReqOntology.AUCTION);
		myAgent.send(message);
	}

	/**
	 * 
	 * Inform about end of the auction
	 * 
	 */
	private void informEnd() {

		ACLMessage message = new ACLMessage(ACLMessage.INFORM);

		for (AID participant : participants) {
			message.addReceiver(participant);
		}

		try {
			message.setContentObject(auction);
		} catch (IOException e) {
			e.printStackTrace();
		}

		message.setOntology(ReqOntology.AUCTION);
		myAgent.send(message);
	}

	private void informNewBid() {
		ACLMessage message = new ACLMessage(ACLMessage.CFP);

		for (AID participant : participants) {
			message.addReceiver(participant);
		}

		try {
			message.setContentObject(auction);
		} catch (IOException e) {
			e.printStackTrace();
		}

		message.setOntology(ReqOntology.AUCTION);
		myAgent.send(message);
	}

	/**
	 * 
	 * Find all buyers (participants) aka curator agents
	 * 
	 * @param agent
	 */
	private void findParticipants() {
		DFAgentDescription description = new DFAgentDescription();
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType("buyer");
		description.addServices(serviceDescription);
		try {
			DFAgentDescription[] resultAgentDescriptions = DFService.search(myAgent, description);
			if (resultAgentDescriptions.length > 0) {
				for (int i = 0; i < resultAgentDescriptions.length; i++)
					participants.add(resultAgentDescriptions[i].getName());
			}
		} catch (FIPAException e) {
			e.printStackTrace();
		}
	}

}
