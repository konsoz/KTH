package se.kth.ID1302.Schema.View;

import java.awt.EventQueue;

import javax.swing.JFrame;

import java.awt.GridBagLayout;

import javax.swing.JLabel;

import java.awt.Component;
import java.awt.Event;
import java.awt.GridBagConstraints;
import java.awt.Insets;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SpringLayout;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.AbstractAction;

import java.awt.event.ActionEvent;

import javax.swing.Action;

//import org.jdatepicker.impl.JDatePanelImpl;
//import org.jdatepicker.impl.JDatePickerImpl;
//import org.jdatepicker.impl.UtilDateModel;
















import java.awt.Choice;

import com.toedter.calendar.JDateChooser;
import com.toedter.calendar.JDayChooser;
import com.toedter.calendar.JCalendar;

import java.awt.Button;
import java.awt.event.ActionListener;
import java.awt.Color;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.FileWriter;
import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.swing.border.BevelBorder;
import javax.swing.border.SoftBevelBorder;

import net.fortuna.ical4j.model.component.VEvent;
import se.kth.ID1302.Schema.Controller.Controller;
import se.kth.ID1302.Schema.Model.Algorithm;
import se.kth.ID1302.Schema.Model.EventTree;
import se.kth.ID1302.Schema.Model.VEventContainer;




public class View{
	
	private JFrame frame;
	private JTextField textField;
	private JTextField deltagareFiled;
	private JTextField timeStartField;
	private JTextField timeEndField;
	private JTextField durationField;
	private JTextField maxUnattendanceField;
	private String test; 
	
	private SwingAction exitHandler;
	private OKAction	okHandler;	
	
	private final JCalendar calendar = new JCalendar();
	private final JFileChooser fc = new JFileChooser(); 
	private final Controller controller = new Controller();
	private Algorithm alg;

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					View window = new View();
					window.frame.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	/**
	 * Create the application.
	 */
	public View() {
		initialize();
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		frame = new JFrame();
		frame.setBounds(100, 100, 500, 500);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		SpringLayout springLayout = new SpringLayout();
		frame.getContentPane().setLayout(springLayout);
		
		
		JLabel lblTid = new JLabel("Starttid (HH:MM)");
		springLayout.putConstraint(SpringLayout.WEST, lblTid, 10, SpringLayout.WEST, frame.getContentPane());
		frame.getContentPane().add(lblTid);
		
		timeStartField = new JTextField();
		springLayout.putConstraint(SpringLayout.NORTH, timeStartField, 25, SpringLayout.NORTH, frame.getContentPane());
		springLayout.putConstraint(SpringLayout.NORTH, lblTid, 2, SpringLayout.NORTH, timeStartField);
		springLayout.putConstraint(SpringLayout.EAST, timeStartField, -71, SpringLayout.EAST, frame.getContentPane());
		frame.getContentPane().add(timeStartField);
		timeStartField.setColumns(10);
		
		JLabel lblNewLabel_2 = new JLabel("Sluttid (HH:MM)");
		springLayout.putConstraint(SpringLayout.WEST, lblNewLabel_2, 0, SpringLayout.WEST, lblTid);
		frame.getContentPane().add(lblNewLabel_2);
		
		timeEndField = new JTextField();
		springLayout.putConstraint(SpringLayout.NORTH, lblNewLabel_2, 2, SpringLayout.NORTH, timeEndField);
		springLayout.putConstraint(SpringLayout.NORTH, timeEndField, 6, SpringLayout.SOUTH, timeStartField);
		springLayout.putConstraint(SpringLayout.WEST, timeEndField, 157, SpringLayout.WEST, frame.getContentPane());
		springLayout.putConstraint(SpringLayout.EAST, timeEndField, 0, SpringLayout.EAST, timeStartField);
		frame.getContentPane().add(timeEndField);
		timeEndField.setColumns(10);
		
		JLabel lblMteshd = new JLabel("Mötes längd");
		springLayout.putConstraint(SpringLayout.WEST, lblMteshd, 0, SpringLayout.WEST, lblTid);
		frame.getContentPane().add(lblMteshd);
		
		durationField = new JTextField();
		durationField.setToolTipText("");
		springLayout.putConstraint(SpringLayout.NORTH, lblMteshd, 2, SpringLayout.NORTH, durationField);
		springLayout.putConstraint(SpringLayout.NORTH, durationField, 6, SpringLayout.SOUTH, timeEndField);
		springLayout.putConstraint(SpringLayout.WEST, durationField, 0, SpringLayout.WEST, timeStartField);
		springLayout.putConstraint(SpringLayout.EAST, durationField, -71, SpringLayout.EAST, frame.getContentPane());
		frame.getContentPane().add(durationField);
		durationField.setColumns(10);
		
		JLabel lblJ = new JLabel("Max frånvarande ");
		springLayout.putConstraint(SpringLayout.WEST, lblJ, 0, SpringLayout.WEST, lblTid);
		frame.getContentPane().add(lblJ);
		
		maxUnattendanceField = new JTextField();
		springLayout.putConstraint(SpringLayout.NORTH, maxUnattendanceField, 4, SpringLayout.SOUTH, durationField);
		springLayout.putConstraint(SpringLayout.WEST, maxUnattendanceField, 28, SpringLayout.EAST, lblJ);
		springLayout.putConstraint(SpringLayout.EAST, maxUnattendanceField, -71, SpringLayout.EAST, frame.getContentPane());
		springLayout.putConstraint(SpringLayout.NORTH, lblJ, 2, SpringLayout.NORTH, maxUnattendanceField);
		springLayout.putConstraint(SpringLayout.WEST, timeStartField, 0, SpringLayout.WEST, maxUnattendanceField);
		frame.getContentPane().add(maxUnattendanceField);
		maxUnattendanceField.setColumns(10);
		
		JLabel lblNewLabel = new JLabel("Startdag");
		springLayout.putConstraint(SpringLayout.WEST, lblNewLabel, 0, SpringLayout.WEST, lblTid);
		frame.getContentPane().add(lblNewLabel);
		
		
		JLabel lblNewLabel_1 = new JLabel("Slutdag");
		springLayout.putConstraint(SpringLayout.NORTH, lblNewLabel_1, 148, SpringLayout.NORTH, frame.getContentPane());
		springLayout.putConstraint(SpringLayout.SOUTH, lblNewLabel, -6, SpringLayout.NORTH, lblNewLabel_1);
		springLayout.putConstraint(SpringLayout.WEST, lblNewLabel_1, 0, SpringLayout.WEST, lblTid);
		frame.getContentPane().add(lblNewLabel_1);
		
		final JDateChooser dateChooser = new JDateChooser();
		springLayout.putConstraint(SpringLayout.NORTH, dateChooser, 6, SpringLayout.SOUTH, maxUnattendanceField);
		springLayout.putConstraint(SpringLayout.WEST, dateChooser, 0, SpringLayout.WEST, timeStartField);
		frame.getContentPane().add(dateChooser);
		
		final JDateChooser dateChooser_1 = new JDateChooser();
		springLayout.putConstraint(SpringLayout.NORTH, dateChooser_1, 6, SpringLayout.SOUTH, dateChooser);
		springLayout.putConstraint(SpringLayout.WEST, dateChooser_1, 0, SpringLayout.WEST, timeStartField);
		frame.getContentPane().add(dateChooser_1);
		springLayout.putConstraint(SpringLayout.NORTH, calendar, 64, SpringLayout.SOUTH, dateChooser_1);
		springLayout.putConstraint(SpringLayout.EAST, calendar, -147, SpringLayout.EAST, frame.getContentPane());
		
		
		calendar.addPropertyChangeListener("calendar", new PropertyChangeListener() {

		    @Override
		    public void propertyChange(PropertyChangeEvent e) {
		        if (alg != null)
		        	redrawCalendar();
		    }
		});
		
		calendar.getDayChooser().addPropertyChangeListener("day", new PropertyChangeListener() {

		    @Override
		    public void propertyChange(PropertyChangeEvent e) {
		        Date date = calendar.getDate();
		        Date clickedOnDate = new Date(date.getYear(), date.getMonth(), date.getDate(),0,0,0);
		      try {
		    	  List<VEvent> list = alg.getPossibleMeetings().get(clickedOnDate);
		    	  VEventContainer bigList[] = new VEventContainer[list.size()];
			        for (int i = 0; i < bigList.length; i++) {
			        	bigList[i] = new VEventContainer(list.get(i));
			        }
			        VEventContainer returned = (VEventContainer) JOptionPane.showInputDialog(frame, "Välj mötestid\n(Lägre prioritet bättre)", clickedOnDate + "", JOptionPane.QUESTION_MESSAGE,
			              null, bigList, bigList[0]);
			        if (returned == null) return;
			        
			        saveFile(returned.getVEvent());
			    
			        
			        
		      } catch(NullPointerException exp) {
		    	  JOptionPane.showMessageDialog(frame, "Inga möten denna dag!", "Hoppsan!" , JOptionPane.ERROR_MESSAGE);
		      }
		    }
		});
		
		calendar.getDayChooser().getDayPanel().setBorder(new SoftBevelBorder(BevelBorder.LOWERED, null, null, null, null));
		calendar.getDayChooser().getDayPanel().setBackground(new Color(51, 102, 153));
		calendar.getDayChooser().getDayPanel().setForeground(Color.RED);
		frame.getContentPane().add(calendar);
		
		JButton btnNewButton = new JButton("Avbryt\n");
		springLayout.putConstraint(SpringLayout.WEST, calendar, 0, SpringLayout.WEST, btnNewButton);
		springLayout.putConstraint(SpringLayout.SOUTH, calendar, -20, SpringLayout.NORTH, btnNewButton);
		springLayout.putConstraint(SpringLayout.WEST, btnNewButton, 32, SpringLayout.WEST, frame.getContentPane());
		springLayout.putConstraint(SpringLayout.SOUTH, btnNewButton, -10, SpringLayout.SOUTH, frame.getContentPane());
		frame.getContentPane().add(btnNewButton);
		exitHandler = new SwingAction();
		btnNewButton.addActionListener(exitHandler);
		
		JButton btnOk = new JButton("Klar");
		springLayout.putConstraint(SpringLayout.NORTH, btnOk, 0, SpringLayout.NORTH, btnNewButton);
		springLayout.putConstraint(SpringLayout.EAST, btnOk, -50, SpringLayout.EAST, frame.getContentPane());
		frame.getContentPane().add(btnOk);
		
		JButton btnImport = new JButton("Importera");
		springLayout.putConstraint(SpringLayout.NORTH, btnImport, 21, SpringLayout.SOUTH, lblNewLabel_1);
		springLayout.putConstraint(SpringLayout.EAST, btnImport, 0, SpringLayout.EAST, btnNewButton);
		btnImport.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String owner;
				String path;
				int returnVal;
				try {
					owner = JOptionPane.showInputDialog("Namn på deltagare:");
					returnVal = fc.showOpenDialog(frame);
					
					path = fc.getSelectedFile().getAbsolutePath();
					try {
						controller.importSchema(owner, path);
					} catch (ParseException e1) {
						JOptionPane.showMessageDialog(frame, "Fel vid Parsing", "Error!" , JOptionPane.ERROR_MESSAGE);
					}
				} catch(NullPointerException exception) {
						
				}
			}
		});
		frame.getContentPane().add(btnImport); 
		
		JButton btnNewButton_1 = new JButton("Kör");
		btnNewButton_1.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					Date startdate = new Date(dateChooser.getDate().getYear(), dateChooser.getDate().getMonth(), dateChooser.getDate().getDate());
					Date enddate   = new Date(dateChooser_1.getDate().getYear(), dateChooser_1.getDate().getMonth(), dateChooser_1.getDate().getDate());
					String timeStartString = timeStartField.getText();
					int timmar  = Integer.parseInt(timeStartString.substring(0, 2));
					int minuter = Integer.parseInt(timeStartString.substring(3, 5));
					Date timeStart = new Date (0, 0, 0, timmar, minuter);
					
					String timeEndString = timeEndField.getText();
					timmar  = Integer.parseInt(timeEndString.substring(0, 2));
					minuter = Integer.parseInt(timeEndString.substring(3, 5));
					Date timeEnd = new Date (0, 0, 0, timmar, minuter);
					
					int duration = Integer.parseInt(durationField.getText());
					
					int maxUnattendance = Integer.parseInt(maxUnattendanceField.getText());
					
					if (timeStart.after(timeEnd)) 
						JOptionPane.showMessageDialog(frame, "Starttid är efter sluttid", "Error!" , JOptionPane.ERROR_MESSAGE);
					else {
					if (startdate.after(enddate))
						JOptionPane.showMessageDialog(frame, "Startdatum är efter slutdatum", "Error!" , JOptionPane.ERROR_MESSAGE);
					else {
					if (!(duration > 0)) 
						JOptionPane.showMessageDialog(frame, "Felaktig längd", "Error!" , JOptionPane.ERROR_MESSAGE);
					else {
					if (maxUnattendance < 0) 
						JOptionPane.showMessageDialog(frame, "Negativ max frånvarande", "Error!" , JOptionPane.ERROR_MESSAGE);
					else {
						alg = controller.runAlgorithm(startdate, enddate, timeStart, timeEnd, duration, maxUnattendance);
						redrawCalendar();					
					}}}}
				} catch (NullPointerException excpt) {
					JOptionPane.showMessageDialog(frame, "Du måste ange alla parametrar först!", "Hoppsan!" , JOptionPane.ERROR_MESSAGE);
				}
			}
		});
		springLayout.putConstraint(SpringLayout.NORTH, btnNewButton_1, 0, SpringLayout.NORTH, btnImport);
		springLayout.putConstraint(SpringLayout.WEST, btnNewButton_1, 9, SpringLayout.EAST, btnImport);
		springLayout.putConstraint(SpringLayout.EAST, btnNewButton_1, -293, SpringLayout.EAST, frame.getContentPane());
		frame.getContentPane().add(btnNewButton_1);
		
		
		
		okHandler = new OKAction(); 
		btnOk.addActionListener(okHandler);
		
	}
	
	private void redrawCalendar() {
		Calendar cal = Calendar.getInstance();
		cal.setTime(calendar.getDate());
		int day = cal.get(Calendar.DAY_OF_MONTH);
		int month = cal.get(Calendar.MONTH);
		int year = cal.get(Calendar.YEAR);
		
		JPanel jpanel = calendar.getDayChooser().getDayPanel();
		Component[] components = jpanel.getComponents();
		EventTree<Date, VEvent> treee = alg.getPossibleMeetings();
		
		for(Date date : treee.keys()) { // För varje dag
		    //selected month and year on JCalendar
			if(month == date.getMonth() && year == date.getYear()+1900) {
				
		         // Calculate the offset of the first day of the month
		         cal.set(Calendar.DAY_OF_MONTH,1);
		         int offset = cal.get(Calendar.DAY_OF_WEEK) - 1;

		         //this value will differ from each month due to first days of each month
		         components[date.getDate() + offset + 6].setBackground(Color.green);
		         
		         //components[date.getDate() + offset].setForeground(Color.red); 
		    }
		}
	}

	public String gettextField(){
	return textField.getText(); 
	}
	
	private class SwingAction implements ActionListener {
		
		public void actionPerformed(ActionEvent e) {
			System.exit(0);
		}
	}
	private class OKAction implements ActionListener{
			@Override
		public void actionPerformed(ActionEvent e) {
				frame.dispose();			
		}	
	}
	
	public void saveFile(VEvent event) {
	   int retrival = fc.showSaveDialog(null);
	    if (retrival == JFileChooser.APPROVE_OPTION) {
	    	controller.exportEvents(event,(fc.getSelectedFile()+".ics"));
	    }
	}
}

