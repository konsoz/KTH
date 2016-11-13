package se.kth.ID1302.Schema.Model;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Date;
import java.util.List;

import net.fortuna.ical4j.model.component.VEvent;
import net.fortuna.ical4j.model.property.CalScale;
import net.fortuna.ical4j.model.property.ProdId;
import net.fortuna.ical4j.model.Calendar;

public class Exporter {
	
	public static void createCalender(VEvent event, String path) {
		Calendar icsCalendar = new Calendar();
		icsCalendar.getProperties().add(new ProdId("-//Events Calendar//iCal4j 1.0//EN"));
		icsCalendar.getProperties().add(CalScale.GREGORIAN);

		
		icsCalendar.getComponents().add(event);
	
		File file = new File(path);
		try {
			FileWriter writer = new FileWriter(file);
			writer.write(icsCalendar.toString());
			writer.close();
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		
		
		System.out.println(icsCalendar);
		
	}

}
