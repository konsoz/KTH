/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package se.kth.ict.IV1201.project.model;

import java.io.Serializable;
import java.util.Locale;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 *
 * @author daniel
 */
@Entity
@Table(name = "LOCALIZATION")
public class Localization implements Serializable {

    private static final long serialVersionUID = 1L;
    
    @Id
    @Column(name = "LOCALEKEY")
    private String localeKey;
    @Column(name = "EN")
    private String en;
    @Column(name = "SV")
    private String sv;
    @Column(name = "ES")
    private String es;

    public Localization() {
    }
    
    public String getTranslation(Locale locale) {
        if (locale.getLanguage().equals("es"))
            return es;
        else if(locale.getLanguage().equals("sv"))
            return sv;
        else 
            return en;
    }
    
}
