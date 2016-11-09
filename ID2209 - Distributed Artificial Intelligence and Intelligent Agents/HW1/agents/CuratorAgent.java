package agents;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import jade.core.Agent;
import jade.core.behaviours.OneShotBehaviour;
import jade.core.behaviours.ParallelBehaviour;
import jade.core.behaviours.SequentialBehaviour;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.FailureException;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.lang.acl.UnreadableException;
import jade.proto.SimpleAchieveREResponder;
import models.Artifact;
import models.ReqOntology;
import models.User;

public class CuratorAgent extends Agent {
	
	private List <Artifact> artCollection;
	
	@Override
	protected void setup(){
		System.out.println("Curator Agent "+getAID().getName()+" is ready");
		
		// Register ourself
		register();
		
		// Create art collection
		
		SequentialBehaviour sequentialBehaviour = new SequentialBehaviour();

		sequentialBehaviour.addSubBehaviour(new OneShotBehaviour() {
			
			@Override
			public void action() {
				artCollection = new ArrayList<>();
				for (int i = 0; i < 100; i++) {
					Artifact artifact = new Artifact();
					artCollection.add(artifact);
				}
				
			}
		});
		
		ParallelBehaviour parallelBehaviour = new ParallelBehaviour();
		
		//Add responder behaviours for building a tour and getting artifact details.
		MessageTemplate buildTourTemplate = MessageTemplate.MatchOntology(ReqOntology.REQUEST_BUILD_TOUR);
		MessageTemplate requestTourDetailsTemplate = MessageTemplate.MatchOntology(ReqOntology.REQUEST_ARTIFACT_INFO);
		
		parallelBehaviour.addSubBehaviour(new BuildTourResponder(this, buildTourTemplate));
		parallelBehaviour.addSubBehaviour(new TourDetailsResponder(this, requestTourDetailsTemplate));
		
		sequentialBehaviour.addSubBehaviour(parallelBehaviour);
		
		addBehaviour(sequentialBehaviour);
		
		
	}
	
	
	private void register(){
		DFAgentDescription description = new DFAgentDescription();
		description.setName(getAID());
		ServiceDescription serviceDescription = new ServiceDescription();
		serviceDescription.setType("curator");
		serviceDescription.setName(getName());
		description.addServices(serviceDescription);
		try {
			DFService.register(this, description);
		} catch (FIPAException e) {
			e.printStackTrace();
		}
	}
	
	private ArrayList<Long> getTourForUser(User user){
		
		ArrayList<Long> idsToDeliver = new ArrayList<>();
		
		for (Artifact artifact : artCollection) {
			if(artifact.getMoreInterestingFor().equals(user.getGender())
					&& (Math.abs(artifact.getMoreInterestingAge() - user.getAge()) <= 10)){
				idsToDeliver.add(artifact.getId());
			}
		}
		
		return idsToDeliver;
	}
	
	private ArrayList<Artifact> getArtifactsWithIds(ArrayList<Long> ids){
		ArrayList<Artifact> artifactsToDeliver = new ArrayList<>();
		for (Long id : ids) {
			for (Artifact artifact : artCollection) {
				if(id==artifact.getId()) artifactsToDeliver.add(artifact);
			}
		}
		
		return artifactsToDeliver;
	}
	
	
	class BuildTourResponder extends SimpleAchieveREResponder {

		public BuildTourResponder(Agent a, MessageTemplate mt) {
			super(a, mt);
		}

		
		@Override
		protected ACLMessage prepareResponse(ACLMessage request){
			return null;
		}
		
		@Override
		protected ACLMessage prepareResultNotification(ACLMessage request,
				ACLMessage response) throws FailureException {
			System.out.println("Curator received build tour request from guide"); 

			ACLMessage informDone = request.createReply();
			informDone.setPerformative(ACLMessage.INFORM);
			
			try {
				User user = (User) request.getContentObject();
				ArrayList<Long> ids = getTourForUser(user);
				informDone.setContentObject(ids);
			} 
			catch (IOException | UnreadableException e) {
				e.printStackTrace();
			}
			
			return informDone;
		}
	}

	
	class TourDetailsResponder extends SimpleAchieveREResponder {

		public TourDetailsResponder(Agent a, MessageTemplate mt) {
			super(a, mt);
		}
		
		@Override
		protected ACLMessage prepareResponse(ACLMessage request){
			return null;
		}

		@Override
		protected ACLMessage prepareResultNotification(ACLMessage request,
				ACLMessage response) throws FailureException {
			System.out.println("Curator received tour details request from profiler"); 
			
			ACLMessage informDone = request.createReply();
			informDone.setPerformative(ACLMessage.INFORM);
			
			try {
				@SuppressWarnings("unchecked")
				ArrayList<Long> ids = (ArrayList<Long>) request.getContentObject();
				ArrayList<Artifact> artifacts = getArtifactsWithIds(ids);
				informDone.setContentObject(artifacts);
			} 
			catch (IOException | UnreadableException e) {
				e.printStackTrace();
			}
			
			return informDone;
		}
	}
}
