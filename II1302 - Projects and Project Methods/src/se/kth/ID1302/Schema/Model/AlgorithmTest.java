package se.kth.ID1302.Schema.Model;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import net.fortuna.ical4j.model.DateTime;
import net.fortuna.ical4j.model.component.VEvent;

import org.junit.Test;

/**
 * Testa om algoritm fungerar genom att kolla om det skapas bara 1 möjlig tid för möte vid angiven tid
 * samt testa om algoritm fungerar när det inte finns några möjliga tider för att ha ett möte.
 * 
 * @author Grupp 1
 *
 */

public class AlgorithmTest {

	@Test
	public void testAlgorithm() {
		EventTree<Date, Event> tree = new EventTree<Date, Event>();
		tree.put(new Date (0,0,0,0,0), 
				 new Event(new Date(0,0,0,8,30), new Date(0,0,0,9,0), ""));
		Date dateStart = new Date(0,0,0,0,0);
		Date dateEnd   = new Date(0,0,0,0,0);
		
		Date timeStart = new Date(0,0,0,8,0);
		Date timeEnd   = new Date(0,0,0,9,0);
		
		int duration   = 30;
		int maxUnattendance = 0;

		Algorithm alg  = new Algorithm(tree, dateStart, dateEnd, timeStart, timeEnd, duration, maxUnattendance);
		
		EventTree<Date, VEvent> actual  = alg.getPossibleMeetings();
		List<VEvent> expected = new ArrayList<VEvent>();

		DateTime start = new DateTime(new Date(0,0,0,8,0));
		DateTime end   = new DateTime(new Date(0,0,0,8,30));

		start.setUtc(true);
		end.setUtc(true);
		
		expected.add(new VEvent(start, end, ""));
		
		assertEquals(expected, actual);

		duration   = 60;
		alg  = new Algorithm(tree, dateStart, dateEnd, timeStart, timeEnd, duration, maxUnattendance);
		
		actual   = alg.getPossibleMeetings();
		expected = new ArrayList<VEvent>();
		
		assertEquals(expected, actual);
	}

}
