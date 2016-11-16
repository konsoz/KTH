package se.kth.ID1302.Schema.Controller;

import java.text.ParseException;
import java.util.Date;




import java.util.List;

import net.fortuna.ical4j.model.component.VEvent;
import se.kth.ID1302.Schema.Model.EventTree;
import se.kth.ID1302.Schema.Model.Exporter;
import se.kth.ID1302.Schema.Model.Importer;
import se.kth.ID1302.Schema.Model.Event;
import se.kth.ID1302.Schema.Model.Algorithm;

public class Controller {
	EventTree<Date, Event> tree = new EventTree<Date, Event>();
	
	public void importSchema(String owner, String nameOfFile) throws ParseException {
		Importer.importCalendar(tree, owner, nameOfFile);
	}
	
	public Algorithm runAlgorithm(
			Date dateStart, Date dateEnd, 
			Date timeStart, Date timeEnd, int duration, int maxUnattendance) {
		
		Algorithm alg = new Algorithm(tree, dateStart, dateEnd, timeStart, timeEnd, duration, maxUnattendance);
		return alg;
	}
	
	public void exportEvents(VEvent event, String path) {
		Exporter.createCalender(event, path);
	}
}
