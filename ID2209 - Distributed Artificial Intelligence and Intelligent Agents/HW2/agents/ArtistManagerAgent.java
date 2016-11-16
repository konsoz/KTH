package agents;

import behaviors.AuctioneerBehavior;
import jade.core.Agent;
import models.Artifact;
import models.Auction;

public class ArtistManagerAgent extends Agent {
	
	private static int PRICE = 1500;
	
	
	@Override
	protected void setup(){
		System.out.println("Artist manager agent "+getAID().getName()+" is ready");
		
		Auction auction = new Auction(new Artifact(), PRICE);
		
		addBehaviour(new AuctioneerBehavior(this, auction));
	}

}
