package se.kth.ict.IV1201.project.model;

import java.util.Date;
import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import se.kth.ict.IV1201.project.model.Person;

@Generated(value="EclipseLink-2.5.2.v20140319-rNA", date="2016-03-10T14:37:26")
@StaticMetamodel(Availability.class)
public class Availability_ { 

    public static volatile SingularAttribute<Availability, Date> fromDate;
    public static volatile SingularAttribute<Availability, Long> availabilityId;
    public static volatile SingularAttribute<Availability, Date> toDate;
    public static volatile SingularAttribute<Availability, Person> personId;

}