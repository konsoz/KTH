package agents;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import behaviors.BuyerBehavior;
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
		System.out.println("Curator Agent (Buyer) "+getAID().getName()+" is ready");
		
		addBehaviour(new BuyerBehavior(this));
		
		// Register ourself
		//register();
		
		// Create art collection
		
		/**
		SequentialBehaviour sequentialBehaviour = new SequentialBehaviour();
		
		// Just to meet homework requirements...
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
		*/
		// Create parallel behavior for getting a tour and retrieving tour artifacts
		// it terminates when a particular condition on its sub-behaviours is met 
		// i.e. when all children are done, N children are done or any child is done.
		/**
		ParallelBehaviour parallelBehaviour = new ParallelBehaviour();
		MessageTemplate tourTemplate = MessageTemplate.MatchOntology(ReqOntology.REQUEST_BUILD_TOUR);
		MessageTemplate tourDetailsTemplate = MessageTemplate.MatchOntology(ReqOntology.REQUEST_ARTIFACT_INFO);
		
		parallelBehaviour.addSubBehaviour(new TourResponder(this, tourTemplate));
		parallelBehaviour.addSubBehaviour(new TourDetailsResponder(this, tourDetailsTemplate));
		
		sequentialBehaviour.addSubBehaviour(parallelBehaviour);
		
		addBehaviour(sequentialBehaviour);
		**/
		
		
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
	
	/**
	 * 
	 * Get tour ids based on user's gender and age
	 * 
	 * @param user
	 * @return tour ids
	 */
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
	
	/**
	 * 
	 * Get artifacts description based on artifact id
	 * 
	 * @param ids
	 * @return artifacts
	 */
	private ArrayList<Artifact> getArtifactsWithIds(ArrayList<Long> ids){
		ArrayList<Artifact> artifactsToDeliver = new ArrayList<>();
		for (Long id : ids) {
			for (Artifact artifact : artCollection) {
				if(id==artifact.getId()) artifactsToDeliver.add(artifact);
			}
		}
		
		return artifactsToDeliver;
	}
	
	/**
	 * 
	 * Get tour build request from guide and reply with artifact ids
	 * 
	 * @author konstantin
	 *
	 */
	class TourResponder extends SimpleAchieveREResponder {

		public TourResponder(Agent a, MessageTemplate mt) {
			super(a, mt);
		}

		// Must override otherwise getting error message (?)
		@Override
		protected ACLMessage prepareResponse(ACLMessage request){
			return null;
		}
		
		@Override
		protected ACLMessage prepareResultNotification(ACLMessage request,
				ACLMessage response) throws FailureException {
			System.out.println("Curator received build tour request from guide..."); 

			ACLMessage reply = request.createReply();
			reply.setPerformative(ACLMessage.INFORM);
			
			try {
				User user = (User) request.getContentObject();
				ArrayList<Long> ids = getTourForUser(user);
				reply.setContentObject(ids);
			} 
			catch (IOException | UnreadableException e) {
				e.printStackTrace();
			}
			
			return reply;
		}
	}

	/**
	 * 
	 * 
	 * Wait for tour details request from profiler and build artifact list based on artifact ids
	 * 
	 * @author konstantin
	 *
	 */
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
			System.out.println("Curator received tour details request from profiler..."); 
			
			ACLMessage reply = request.createReply();
			reply.setPerformative(ACLMessage.INFORM);
			
			try {
				@SuppressWarnings("unchecked")
				ArrayList<Long> ids = (ArrayList<Long>) request.getContentObject();
				ArrayList<Artifact> artifacts = getArtifactsWithIds(ids);
				reply.setContentObject(artifacts);
			} 
			catch (IOException | UnreadableException e) {
				e.printStackTrace();
			}
			
			return reply;
		}
	}
}
