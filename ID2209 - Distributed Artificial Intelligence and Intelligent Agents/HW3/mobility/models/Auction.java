package mobility.models;

import java.io.Serializable;

import jade.core.AID;

public class Auction implements Serializable {
	
	public enum Status {
		ONGOING,
		NOT_STARTED,
		FINISHED_SOLD,
		FINISHED_NOT_SOLD
	}
	
	private Artifact artifact;
	private double currentBid;
	private double minSellPrice;
	private double realPrice;
	private Status currentStatus;
	private AID buyer;
	private double proposal;
	
	
	public Auction(Artifact artifact, double realPrice) {
		this.artifact = artifact;
		currentBid = realPrice*2;
		this.realPrice = realPrice;
		setMinSellPrice(realPrice * 0.9);
	}
	
	public Artifact getArtifact() {
		return artifact;
	}
	public void setArtifact(Artifact artifact) {
		this.artifact = artifact;
	}
	public double getCurrentBid() {
		return currentBid;
	}
	public void setCurrentBid(double currentBid) {
		this.currentBid = currentBid;
	}
	public double getRealPrice() {
		return realPrice;
	}
	public void setRealPrice(double realPrice) {
		this.realPrice = realPrice;
	}
	public Status getCurrentStatus() {
		return currentStatus;
	}
	public void setCurrentStatus(Status currentStatus) {
		this.currentStatus = currentStatus;
	}

	public double getMinSellPrice() {
		return minSellPrice;
	}

	public void setMinSellPrice(double minSellPrice) {
		this.minSellPrice = minSellPrice;
	}

	public AID getBuyer() {
		return buyer;
	}

	public void setBuyer(AID buyer) {
		this.buyer = buyer;
	}

	public double getProposal() {
		return proposal;
	}

	public void setProposal(double proposal) {
		this.proposal = proposal;
	}

}
