package mobility.agents;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import mobility.behaviors.BuyerBehavior;
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
import mobility.models.Artifact;
import mobility.models.ReqOntology;
import mobility.models.User;

public class ParticipantAgent extends Agent {
	
	private List <Artifact> artCollection;
	
	@Override
	protected void setup(){
		System.out.println("Buyer "+getAID().getName()+" is ready");
		
		addBehaviour(new BuyerBehavior(this));
				
	}
}
