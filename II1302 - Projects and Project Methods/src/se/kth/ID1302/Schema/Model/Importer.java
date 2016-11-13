package se.kth.ID1302.Schema.Model;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import net.fortuna.ical4j.data.CalendarBuilder;
import net.fortuna.ical4j.data.ParserException;
import net.fortuna.ical4j.model.Calendar;
import net.fortuna.ical4j.model.Component;
import net.fortuna.ical4j.model.Property;
import net.fortuna.ical4j.model.TimeZone;

public class Importer {
	
	public static void importCalendar(EventTree<Date, Event> eventTree, 
			String owner, String nameOfFile){
		FileInputStream fin;
		CalendarBuilder builder;
		Calendar calendar = null;
		
		try {
			fin = new FileInputStream(nameOfFile);
			builder = new CalendarBuilder();
			calendar = builder.build(fin);
			
		} catch (ParserException e) {
			System.err.println("Parse Exception");
		} catch (FileNotFoundException e) {
			System.err.println("Cannot read the file");
		} catch (IOException e) {
			System.err.println("I/O Exception");
		}
		
		
		
		for (Object c : calendar.getComponents()) {
		    Component component = (Component) c;
		    
		    String start = component.getProperties().getProperty(Property.DTSTART).getValue();
		    String end   = component.getProperties().getProperty(Property.DTEND).getValue();
		    
		    DateFormat test = new SimpleDateFormat("yyyyMMdd'T'HHmmss'Z'");
		    test.setTimeZone(TimeZone.getTimeZone("UTC"));
		  	Date startDate = new Date();
		  	Date endDate   = new Date();
			try {
				startDate  = test.parse(start);
				endDate    = test.parse(end);
				
			} catch (ParseException e) {
				e.printStackTrace();
				System.err.println("Date parse Exception");
			}
		    

		    Date key = new Date(startDate.getYear(), startDate.getMonth(), startDate.getDate());
		    
		    Event event = new Event(startDate, endDate, owner);

		    eventTree.put(key, event);
		    
		}
		
	}

}
