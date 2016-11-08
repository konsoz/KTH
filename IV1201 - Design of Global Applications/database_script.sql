INSERT INTO localization (localekey,en,es,sv) VALUES ('Korvgrillning', 'Sausage grilling', 'Assala de saisasage', 'Korvgrillning');
INSERT INTO localization (localekey,en,es,sv) VALUES ('Karuselldrift', 'Carousel managment', 'Carousello el drifto', 'Karuselldrift');
INSERT INTO localization (localekey,en,es,sv) VALUES ('applicant', 'Applicant', 'applicanto', 'sökande');
INSERT INTO localization (localekey,en,es,sv) VALUES ('recruit', 'Recruiter', 'recruto', 'Rekryterare');
INSERT INTO role (role_id, name, username) VALUES (1, 'recruit', 'borg');
INSERT INTO role (role_id, name, username) VALUES (2, 'applicant',null);
INSERT INTO person (person_id, name, surname, username, password, role_id)
VALUES (1, 'Greta', 'Borg', 'borg', 'wl9nk23a', 1);
INSERT INTO person (person_id, name, surname, ssn, email, role_id)
VALUES (2, 'Per', 'Strand', '19671212-1211', 'per@strand.kth.se', 2);
INSERT INTO availability (availability_id, person_id, from_date, to_date)
VALUES (1, 2, '2014-02-23', '2014-05-25');
INSERT INTO availability (availability_id, person_id, from_date, to_date)
VALUES (2, 2, '2014-07-10', '2014-08-10');
INSERT INTO competence (competence_id, name)
VALUES (1, 'Korvgrillning');
INSERT INTO competence (competence_id, name)
VALUES (2, 'Karuselldrift');
INSERT INTO competence_profile (competence_profile_id, person_id,
competence_id, years_of_experience)
VALUES (1, 2, 1, 3.5);
INSERT INTO competence_profile (competence_profile_id, person_id,
competence_id, years_of_experience)
VALUES (2, 2, 2, 2.0);
