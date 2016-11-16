package se.kth.ID1302.Schema.Model;

import java.util.Date;

import net.fortuna.ical4j.model.component.VEvent;

public class VEventContainer {
	private VEvent event;
	
	public VEventContainer(VEvent event) {
		this.event = event;
	}
	
	@Override
	public String toString() {
		Date date = event.getStartDate().getDate();
		int h = date.getHours();
		int m = date.getMinutes();
		String sum = event.getSummary().toString().substring(8).trim();
		
		return (h > 9 ? h : "0" + h) + ":" + (m > 9 ? m : "0" + m) + (sum.length() > 0 ? " - " + sum : "");
	}
	
	public VEvent getVEvent() {
		return event;
	}
}
