package se.kth.ict.IV1201.project.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.CollectionAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import se.kth.ict.IV1201.project.model.Localization;
import se.kth.ict.IV1201.project.model.Person;

@Generated(value="EclipseLink-2.5.2.v20140319-rNA", date="2016-03-10T14:37:26")
@StaticMetamodel(Role.class)
public class Role_ { 

    public static volatile SingularAttribute<Role, Localization> localization;
    public static volatile SingularAttribute<Role, Long> roleId;
    public static volatile CollectionAttribute<Role, Person> personCollection;
    public static volatile SingularAttribute<Role, String> name;
    public static volatile SingularAttribute<Role, String> username;

}