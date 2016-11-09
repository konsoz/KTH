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
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.UnreadableException;
import jade.proto.SimpleAchieveREInitiator;
import models.Artifact;
import models.ReqOntology;
import models.User;

public class ProfilerAgent extends Agent {

	private User user;
	
	private AID curator;
	private AID tourGuide;
	private ArrayList <Long> artifactIDs;
	private ArrayList <Artifact> artifactsToVisit;
	
	@Override
	protected void setup(){
		
		System.out.println("Profiler Agent "+getAID().getName()+" is ready");
		
		artifactIDs = new ArrayList<>();
		artifactsToVisit = new ArrayList<>();
		
		// Create a random user
		
		user = new User();
		
		// Print user for debugg
		
		System.err.println("User: " + user.getAge() + " " + user.getGender());
		
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
		} );
		
		
	}
	
	
	private void getTour() {
		
		// Create request message for tour
		ACLMessage requestTourMessage = new ACLMessage(ACLMessage.REQUEST);
		requestTourMessage.setProtocol(FIPANames.InteractionProtocol.FIPA_REQUEST); 
		requestTourMessage.addReceiver(tourGuide);

		try {
			requestTourMessage.setContentObject(user);
		} catch (IOException e) {
			e.printStackTrace();
		}

		requestTourMessage.setOntology(ReqOntology.REQUEST_TOUR);
		RequestTourInitiator requestTourGuideBehaviour = new RequestTourInitiator(this, requestTourMessage);

		//Request artifact details from the curator.
		ACLMessage requestTourDetailsMessage = new ACLMessage(ACLMessage.REQUEST);
		requestTourDetailsMessage.setProtocol(FIPANames.InteractionProtocol.FIPA_REQUEST); 
		requestTourDetailsMessage.addReceiver(curator);
		requestTourDetailsMessage.setOntology(ReqOntology.REQUEST_ARTIFACT_INFO);
		TourDetailsInitiator requestTourDetailsBehaviour = new TourDetailsInitiator(this, requestTourDetailsMessage);

		//These two behaviors should be executed sequentially,
		//requesting the tour first and then the artifact details.
		SequentialBehaviour sequentialBehaviour = new SequentialBehaviour();
		sequentialBehaviour.addSubBehaviour(requestTourGuideBehaviour);
		sequentialBehaviour.addSubBehaviour(requestTourDetailsBehaviour);
		addBehaviour(sequentialBehaviour);
	}
	
	
	private AID findCurator(){
		DFAgentDescription description = new DFAgentDescription();
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType("curator");
		description.addServices(serviceDescription);
		try {
			DFAgentDescription[] resultAgentDescriptions = DFService.search(this,  description);
			if (resultAgentDescriptions.length > 0) {
				return resultAgentDescriptions[0].getName();
			}
		} 
		catch (FIPAException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	private AID findTourGuide(){
		DFAgentDescription description = new DFAgentDescription();
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType("guide");
		description.addServices(serviceDescription);
		try {
			DFAgentDescription[] resultAgentDescriptions = DFService.search(this,  description);
			if (resultAgentDescriptions.length > 0) {
				return resultAgentDescriptions[0].getName();
			}
		} 
		catch (FIPAException e) {
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
	class RequestTourInitiator extends SimpleAchieveREInitiator {

		public RequestTourInitiator(Agent a, ACLMessage msg) {
			super(a, msg);
		}

		@SuppressWarnings("unchecked")
		@Override
		protected void handleInform(ACLMessage msg) {
			super.handleInform(msg);
			System.out.println("Profiler received tour response from guide");

			try {
				artifactIDs = (ArrayList<Long>) msg.getContentObject();
			} 
			catch (UnreadableException e) {
				e.printStackTrace();
			}
		}
	}
	
	class TourDetailsInitiator extends SimpleAchieveREInitiator {

		public TourDetailsInitiator(Agent a, ACLMessage msg) {
			super(a, msg);
		}

		@Override
		protected ACLMessage prepareRequest(ACLMessage msg) {
			try {
				msg.setContentObject(artifactIDs);
			} 
			catch (IOException e) {
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
					System.out.println(artifact.getId()+" " + artifact.getGenre() +
							" for following gender " + artifact.getMoreInterestingFor()
							+ " age range " + artifact.getMoreInterestingAge());
				}
				
			} 
			catch (UnreadableException e) {
				e.printStackTrace();
			}
		}
	}

}
