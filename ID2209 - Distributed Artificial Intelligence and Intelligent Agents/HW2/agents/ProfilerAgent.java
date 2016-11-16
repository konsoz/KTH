package agents;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import jade.core.AID;
import jade.core.Agent;
import jade.core.behaviours.SequentialBehaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPANames;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.SearchConstraints;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.UnreadableException;
import jade.proto.SimpleAchieveREInitiator;
import jade.proto.SubscriptionInitiator;
import models.Artifact;
import models.ReqOntology;
import models.User;

public class ProfilerAgent extends Agent {

	private User user;

	private AID curator;
	private AID tourGuide;
	private ArrayList<Long> artifactIDs;
	private ArrayList<Artifact> artifactsToVisit;

	@Override
	protected void setup() {

		System.out.println("Profiler Agent " + getAID().getName() + " is ready");

		artifactIDs = new ArrayList<>();
		artifactsToVisit = new ArrayList<>();

		// Create a random user

		user = new User();

		// Print user for debugg

		System.err.println("User: " + user.getAge() + " " + user.getGender());

		addSubscription();
		
		// Find curator and tourguide agents

		addBehaviour(new TickerBehaviour(this, 60000) {
			private static final long serialVersionUID = 1L;

			protected void onTick() {
				System.out.println("Profiler Agent looks for coutrator and tour guide agents...");
				curator = findCurator();
				tourGuide = findTourGuide();
				System.out.println("Profiler Agent tries to get a tour...");
				getTour();

			}
		});

	}

	/**
	 * 
	 * Subscription for new guide services
	 * 
	 */
	@SuppressWarnings("serial")
	private void addSubscription() {
		DFAgentDescription template = new DFAgentDescription();
		ServiceDescription description = new ServiceDescription();
		description.setType("guide");

		SearchConstraints constrains = new SearchConstraints();
		constrains.setMaxResults((long) 1);

		addBehaviour(new SubscriptionInitiator(this,
				DFService.createSubscriptionMessage(this, getDefaultDF(), template, constrains)) {

			@Override
			protected void handleInform(ACLMessage inform) {
				super.handleInform(inform);
				try {
					DFAgentDescription[] resultAgentDescriptions = DFService.decodeNotification(inform.getContent());
					if (resultAgentDescriptions.length > 0) {
						tourGuide = findTourGuide();
					}
				} catch (FIPAException e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * 
	 * Build a tour and request info about artifacts
	 * 
	 */
	private void getTour() {

		// Create request message for tour
		ACLMessage reqMsg = new ACLMessage(ACLMessage.REQUEST);
		reqMsg.setProtocol(FIPANames.InteractionProtocol.FIPA_REQUEST);
		reqMsg.addReceiver(tourGuide);

		try {
			reqMsg.setContentObject(user);
		} catch (IOException e) {
			e.printStackTrace();
		}

		reqMsg.setOntology(ReqOntology.REQUEST_TOUR);
		TourInitiator requestTourGuideBehaviour = new TourInitiator(this, reqMsg);

		// Get tour first and then information about artifacts
		SequentialBehaviour sequentialBehaviour = new SequentialBehaviour();
		sequentialBehaviour.addSubBehaviour(requestTourGuideBehaviour);
		sequentialBehaviour.addSubBehaviour(reqArtifacts());
		addBehaviour(sequentialBehaviour);
	}

	/**
	 * 
	 * Request for artifacts
	 * 
	 * @return
	 */
	private TourDetailsInitiator reqArtifacts() {
		// Request artifact details from the curator.

		ACLMessage reqMsg = new ACLMessage(ACLMessage.REQUEST);
		reqMsg.setProtocol(FIPANames.InteractionProtocol.FIPA_REQUEST);
		reqMsg.addReceiver(curator);
		reqMsg.setOntology(ReqOntology.REQUEST_ARTIFACT_INFO);
		TourDetailsInitiator requestTourDetailsBehaviour = new TourDetailsInitiator(this, reqMsg);
		return requestTourDetailsBehaviour;
	}

	/**
	 * 
	 * Find curator AID from DF Agent
	 * 
	 * @return curator AID
	 */
	private AID findCurator() {
		DFAgentDescription description = new DFAgentDescription();
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType("curator");
		description.addServices(serviceDescription);
		try {
			DFAgentDescription[] resultAgentDescriptions = DFService.search(this, description);
			if (resultAgentDescriptions.length > 0) {
				return resultAgentDescriptions[0].getName();
			}
		} catch (FIPAException e) {
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * 
	 * Find tour guide AID from DF Agent
	 * 
	 * @return
	 */
	private AID findTourGuide() {
		DFAgentDescription description = new DFAgentDescription();
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType("guide");
		description.addServices(serviceDescription);
		try {
			DFAgentDescription[] resultAgentDescriptions = DFService.search(this, description);
			if (resultAgentDescriptions.length > 0) {
				return resultAgentDescriptions[0].getName();
			}
		} catch (FIPAException e) {
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * 
	 * 
	 * Send a request message to tour guide using simpleAchieveREInitiator
	 * 
	 * @author konstantin
	 *
	 */
	class TourInitiator extends SimpleAchieveREInitiator {

		public TourInitiator(Agent a, ACLMessage msg) {
			super(a, msg);
		}

		@SuppressWarnings("unchecked")
		@Override
		protected void handleInform(ACLMessage msg) {
			super.handleInform(msg);
			System.out.println("Profiler received tour response from guide...");

			try {
				artifactIDs = (ArrayList<Long>) msg.getContentObject();
			} catch (UnreadableException e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * 
	 * 
	 * Send a request for retrieving info about artifacts
	 * 
	 * @author konstantin
	 *
	 */
	class TourDetailsInitiator extends SimpleAchieveREInitiator {

		public TourDetailsInitiator(Agent a, ACLMessage msg) {
			super(a, msg);
		}

		@Override
		protected ACLMessage prepareRequest(ACLMessage msg) {
			try {
				msg.setContentObject(artifactIDs);
			} catch (IOException e) {
				e.printStackTrace();
			}

			return super.prepareRequest(msg);
		}

		@SuppressWarnings("unchecked")
		@Override
		protected void handleInform(ACLMessage msg) {
			super.handleInform(msg);

			try {
				artifactsToVisit = (ArrayList<Artifact>) msg.getContentObject();

				System.out.println("Tour with following artifacts is provided by tour and curator agents: ");

				for (Artifact artifact : artifactsToVisit) {
					System.out.println(artifact.getId() + " " + artifact.getGenre() + " for following gender "
							+ artifact.getMoreInterestingFor() + " age range " + artifact.getMoreInterestingAge());
				}

			} catch (UnreadableException e) {
				e.printStackTrace();
			}
		}
	}

}
