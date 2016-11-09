package agents;

import java.io.IOException;

import jade.core.AID;
import jade.core.Agent;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPANames;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.lang.acl.UnreadableException;
import jade.proto.SimpleAchieveREInitiator;
import jade.proto.states.MsgReceiver;
import models.ReqOntology;

public class TourGuideAgent extends Agent {

	private AID curator;
	
	@Override
	protected void setup(){
		
		System.out.println("Tour Guide Agent "+getAID().getName()+" is ready");
		
		
		curator = findCurator();
		
		register();
		
		addMsgReceiver();
		
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
	
	
	private void addMsgReceiver() {
		//Receiving requests for tour from profiler
		MessageTemplate tourRequestMessageTemplate = MessageTemplate.MatchOntology(ReqOntology.REQUEST_TOUR);
		addBehaviour(new MsgReceiver(this, tourRequestMessageTemplate, Long.MAX_VALUE, null, null) {
			
			@Override
			protected void handleMessage(ACLMessage msg) {
				super.handleMessage(msg);
				System.out.println("Guide received tour request from profiler");
				
				//Requests the curator to build a tour
				ACLMessage message = new ACLMessage(ACLMessage.REQUEST);
				message.setProtocol(FIPANames.InteractionProtocol.FIPA_REQUEST); 
				message.addReceiver(curator);
				message.setOntology(ReqOntology.REQUEST_BUILD_TOUR);
				
				try {
					message.setContentObject(msg.getContentObject());
				} 
				catch (IOException | UnreadableException e) {
					e.printStackTrace();
				}
				
				addBehaviour(new BuildTourInitiator(TourGuideAgent.this, message, msg));
			}
			
			@Override
			public int onEnd() {
				//Re-add MsgReceiver
				addMsgReceiver();
				return super.onEnd();
			}
		});
	}
	
	
	private void register(){
		DFAgentDescription description = new DFAgentDescription();
		description.setName(getAID());
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType("guide");
		serviceDescription.setName(getName());
		description.addServices(serviceDescription);
		try {
			DFService.register(this, description);
		} catch (FIPAException e) {
			e.printStackTrace();
		}
	}
	
	
	class BuildTourInitiator extends SimpleAchieveREInitiator {
		ACLMessage original;
		
		public BuildTourInitiator(Agent a, ACLMessage msg, ACLMessage original) {
			super(a, msg);
			this.original = original;
		}

		@Override
		protected void handleInform(ACLMessage msg) {
			super.handleInform(msg);
			System.out.println("Guide received tour built from curator");

			//Sending response back to profiler
			ACLMessage reply = original.createReply();
			reply.setPerformative(ACLMessage.INFORM);
			
			try {
				reply.setContentObject(msg.getContentObject());
			} 
			catch (IOException | UnreadableException e) {
				e.printStackTrace();
			}
			
			send(reply);
		}
	}
	
}
