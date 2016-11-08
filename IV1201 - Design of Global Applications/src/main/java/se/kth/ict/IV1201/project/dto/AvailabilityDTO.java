package se.kth.ict.IV1201.project.dto;

import java.util.Date;

/**
 * This interface is used when a user chooses availability in their application.
 */
public interface AvailabilityDTO {
    
    public void setFromDate(Date fromDate);

    public void setToDate(Date toDate);
    
    @Override
    public String toString();
    
}
