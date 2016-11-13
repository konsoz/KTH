package se.kth.ID1302.Schema.View;

import java.text.ParseException;
import java.util.Date;
import java.util.List;

import net.fortuna.ical4j.model.component.VEvent;
import se.kth.ID1302.Schema.Controller.Controller;
import se.kth.ID1302.Schema.Model.Algorithm;
import se.kth.ID1302.Schema.Model.Event;
import se.kth.ID1302.Schema.Model.EventTree;

public class TempView {

	public static void main(String[] args) {
		try {
			Controller controller = new Controller();
			//EventTree<Date, Event> tree = new EventTree<Date, Event>();
			
			controller.importSchema("Daniel", "personal.ics");
			controller.importSchema("Konstantin","personal2.ics");
			
			// Year(Any) Month(1-12) Day(1-31) Hour(0-23) Min(0-59) Sec MilliS
			
			Algorithm result = controller.runAlgorithm(
					/* Start date: */ new Date(2015 - 1900, 1 - 1, 6),
					/* Stop date:  */ new Date(2015 - 1900, 1 - 1, 10), 
					/* Start time: */ new Date(0,0,0,8,0), 
					/* Stop time:  */ new Date(0,0,0,17,0), 
					/* Duration:   */ 60, 
					/* Max borta:  */ 2);
			for (Date date : result.getPossibleMeetings().keys()) {
				System.out.println(date);
				for (VEvent ve : result.getPossibleMeetings().get(date)) {
					System.out.println(ve);
				}
			}
			//controller.exportEvents(result.getPossibleMeetings(), "");
			
		} catch (ParseException e) { e.printStackTrace(); }
	}
}
