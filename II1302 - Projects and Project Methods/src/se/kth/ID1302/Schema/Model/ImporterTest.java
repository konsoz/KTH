package se.kth.ID1302.Schema.Model;

import static org.junit.Assert.*;

import java.io.File;
import java.util.Date;

import org.junit.Test;

/**
 * Testar om import av filer fungerar genom att kontrollera om längen på filen > 0 
 * samt om filen finns.
 * 
 * Testa även om EventTree skapas med rätt antal event som det finns i filen (69)
 * 
 * @author Grupp 1 
 *
 */

public class ImporterTest {

    @Test
    public void importCalendar(){
    	File file = new File("personal.ics");
        assertTrue(file.toString().length() > 0);
        assertTrue(file.exists());
        
        EventTree<Date, Event> eventTree = new EventTree<Date, Event>();
        Importer.importCalendar(eventTree, "", file.getAbsolutePath());
        
        int expected = 69;
        int actual = eventTree.size();
        assertEquals(expected, actual);
        
        
    }

}
