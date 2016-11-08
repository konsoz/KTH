package se.kth.ict.IV1201.project.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.CollectionAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import se.kth.ict.IV1201.project.model.CompetenceProfile;
import se.kth.ict.IV1201.project.model.Localization;

@Generated(value="EclipseLink-2.5.2.v20140319-rNA", date="2016-03-10T14:37:26")
@StaticMetamodel(Competence.class)
public class Competence_ { 

    public static volatile SingularAttribute<Competence, Localization> localization;
    public static volatile CollectionAttribute<Competence, CompetenceProfile> competenceProfileCollection;
    public static volatile SingularAttribute<Competence, Long> competenceId;
    public static volatile SingularAttribute<Competence, String> name;

}