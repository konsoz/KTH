package se.kth.ID1302.Schema.Model;

import java.util.Date;

public class Event {

	private String owner;
	private Date start;
	private Date end;
	
	public Event(Date start, Date end,String owner) {
		
		this.owner = owner;
		this.start = start;
		this.end = end;
	}

	public Date getStart() {
		return start;
	}

	public Date getEnd() {
		return end;
	}
	
	public String getOwner() {
		return owner;
	}

	public String toString() {
		return "Owner: " + owner +  "\nStart: " + start + "\nEnd: " + end;
	}
}
