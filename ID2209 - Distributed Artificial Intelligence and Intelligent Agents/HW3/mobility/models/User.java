package mobility.models;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import jade.util.leap.Serializable;

public class User implements Serializable {
	private int age;
	private String occupation;
	private String gender; 
	private List<Artifact> visitedArtifacts;
	
	public User() {
		this.age = ThreadLocalRandom.current().nextInt(13, 66);
		this.occupation = Occupation.values()[ThreadLocalRandom.current().nextInt(0, 3)].toString();
		this.gender = Gender.values()[ThreadLocalRandom.current().nextInt(0, 1)].toString();;
		this.visitedArtifacts = new ArrayList<>();
	}

	public int getAge() {
		return age;
	}

	public void setAge(int age) {
		this.age = age;
	}

	public String getOccupation() {
		return occupation;
	}

	public void setOccupation(String occupation) {
		this.occupation = occupation;
	}

	public String getGender() {
		return gender;
	}

	public void setGender(String gender) {
		this.gender = gender;
	}

	public List<Artifact> getVisitedArtifacts() {
		return visitedArtifacts;
	}

	public void setVisitedArtifacts(List<Artifact> visitedArtifacts) {
		this.visitedArtifacts = visitedArtifacts;
	}
	
	public enum Occupation {
		STUDENT, 
		PAINTER,
		MUSICIAN,
		WORKER
		};
		
	public enum Gender {
		MAN,
		WOMAN};
}
