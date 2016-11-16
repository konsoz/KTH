package behaviors;

import java.io.IOException;
import java.util.concurrent.ThreadLocalRandom;

import jade.core.Agent;
import jade.core.behaviours.CyclicBehaviour;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.lang.acl.UnreadableException;
import models.Auction;
import models.ReqOntology;
import models.Auction.Status;

public class BuyerBehavior extends CyclicBehaviour {

	private Double myBid;

	public BuyerBehavior(Agent buyer) {
		super(buyer);

		// Register ourself
		register();

	}

	@Override
	public void action() {

		MessageTemplate mt = MessageTemplate.MatchOntology(ReqOntology.AUCTION);
		ACLMessage message = myAgent.receive(mt);

		if (message != null) {

			Auction auction = null;
			try {
				auction = (Auction) message.getContentObject();
			} catch (UnreadableException e1) {
				e1.printStackTrace();
			}

			setMyBid(auction.getCurrentBid());

			if (auction != null) {
				switch (message.getPerformative()) {

				case ACLMessage.INFORM: {
					//Inform messages, informing of start or end of auction.
					if (auction.getCurrentStatus().equals(Status.ONGOING)) {
						System.out.println(myAgent.getLocalName() + ": Start of auction - " + auction.getCurrentBid());
					}
					else if (auction.getCurrentStatus().equals(Status.FINISHED_SOLD)) {
						System.out.println(myAgent.getLocalName() + ": End of auction - " + auction.getCurrentBid());
					}
					else if (auction.getCurrentStatus().equals(Status.FINISHED_NOT_SOLD)) {
						System.out.println(myAgent.getLocalName() + ": End of auction without bids - " + auction.getCurrentBid());
					}

					break;
				}
				
				case ACLMessage.CFP: {
					if (auction.getCurrentBid() <= myBid) {
						System.out.println(myAgent.getLocalName() + ": Making a proposal to buy item for " + myBid);

						ACLMessage reply = message.createReply();
						reply.setPerformative(ACLMessage.PROPOSE);

						auction.setProposal(myBid);
						
						try {
							reply.setContentObject(auction);
						} catch (IOException e) {
							e.printStackTrace();
						}

						reply.setOntology(ReqOntology.AUCTION);
						myAgent.send(reply);
					}
					// Otherwise dont accept it. (Do nothing)
					else {
						System.out.println(myAgent.getLocalName() + ": Not accepting bid in auction - "
								+ auction.getCurrentBid() + " my bid is lower - " + myBid);
					}
					break;
				}
				case ACLMessage.ACCEPT_PROPOSAL: {
					// The bid the buyer proposed was accepted.
					System.out.println(myAgent.getLocalName() + ": Proposal accepted in auction - " + auction.getCurrentBid());
					break;
				}

				case ACLMessage.REJECT_PROPOSAL: {
					// The bid the buyer proposed was rejected.
					System.out.println(myAgent.getLocalName() + ": Proposal rejected in auction - " + auction.getCurrentBid());
					break;
				}

				default: {break;}
				}

			}
		} else {
			block();
		}
	}

	private void setMyBid(Double firstBid) {
		if (myBid == null) {

			int random = ThreadLocalRandom.current().nextInt(0, 6);

			double proposePrice = firstBid / 2 + random * 75;

			myBid = new Double(proposePrice);
		}
	}

	private void register() {
		DFAgentDescription description = new DFAgentDescription();
		description.setName(myAgent.getAID());
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType("buyer");
		serviceDescription.setName(myAgent.getName());
		description.addServices(serviceDescription);
		try {
			DFService.register(myAgent, description);
		} catch (FIPAException e) {
			e.printStackTrace();
		}
	}

}
