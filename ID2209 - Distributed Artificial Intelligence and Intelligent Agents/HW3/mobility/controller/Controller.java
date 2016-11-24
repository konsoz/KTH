package mobility.controller;
import jade.wrapper.AgentContainer;
import jade.wrapper.AgentController;
import mobility.agents.ArtistManagerAgent;
import mobility.agents.ParticipantAgent;
import jade.core.ProfileImpl;
import jade.core.Runtime;

public class Controller {
		
	public static void main(String[] args){
		
		try {
			// Runtime for creating containers and agents
			Runtime rt = Runtime.instance();
			rt.setCloseVM(true);
			
			// "Main" container
			AgentContainer agentContainer = rt.createMainContainer(new ProfileImpl("localhost",8080,null));
			AgentController mainController = agentContainer.createNewAgent("rma", jade.tools.rma.rma.class.getName(), new Object[0]);
			mainController.start();
			
			// Heritage malta container and agent
			AgentContainer hmContainer = rt.createAgentContainer(new ProfileImpl("localhost", 8080, "hmContainer"));
			AgentController hmAgentController = hmContainer.createNewAgent("hmCurator", ParticipantAgent.class.getName(), new Object[0]);
			hmAgentController.start();
			
			// Galileo container and agent
			AgentContainer galileoContainer = rt.createAgentContainer(new ProfileImpl("localhost", 8080, "galileoContainer"));
			AgentController galileoAgentController = galileoContainer.createNewAgent("galileoCurator", ParticipantAgent.class.getName(), new Object[0]);
			galileoAgentController.start();
			
			// Artist (auctionner) agent and controller
			AgentContainer artistManagerAgentContainer = rt.createAgentContainer(new ProfileImpl("localhost", 8080, "artistManagerContainer"));
			AgentController artistManagerAgentController = artistManagerAgentContainer.createNewAgent("artistManager", ArtistManagerAgent.class.getName(), new Object[0]);
			artistManagerAgentController.start();


			
		} catch(Throwable t) {
			t.printStackTrace();
		}
	}

}
