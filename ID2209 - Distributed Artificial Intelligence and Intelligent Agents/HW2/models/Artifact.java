package models;

import java.util.concurrent.ThreadLocalRandom;

import jade.util.leap.Serializable;
import models.User.Gender;

public class Artifact implements Serializable {
	
	public enum Genre {
		PAINTING, 
		SCULPTURE,
		PHOTOGRAPHY};
	
	private int moreInterestingAge;
	private String moreInterestingFor;
	private long id;
	private String name;
	private String creator;
	private String genre;
	
	public Artifact(){
		id = (long) (Math.random() * Long.MAX_VALUE);
		name = "Artifact " + id;
		creator = "Creator " + id;
		genre = Genre.values()[ThreadLocalRandom.current().nextInt(0, 3)].toString();
		moreInterestingAge = ThreadLocalRandom.current().nextInt(13, 66);
		moreInterestingFor = Gender.values()[ThreadLocalRandom.current().nextInt(0, 1)].toString();
		
	}

	public int getMoreInterestingAge() {
		return moreInterestingAge;
	}

	public void setMoreInterestingAge(int moreInterestingAge) {
		this.moreInterestingAge = moreInterestingAge;
	}

	public String getMoreInterestingFor() {
		return moreInterestingFor;
	}

	public void setMoreInterestingFor(String moreInterestingFor) {
		this.moreInterestingFor = moreInterestingFor;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getCreator() {
		return creator;
	}

	public void setCreator(String creator) {
		this.creator = creator;
	}

	public String getGenre() {
		return genre;
	}

	public void setGenre(String genre) {
		this.genre = genre;
	}
}
