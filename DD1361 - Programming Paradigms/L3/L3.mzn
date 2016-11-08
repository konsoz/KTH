include "globals.mzn";

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VÄRDEN SOM DEFINIERAS I DATAFILERNA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Antal labb-lösningar som ska redovisas
int: solutions;

% Vilken labb som ska redovisas för varje labb-lösning
array [Solutions] of int: solution_lab;

% Vilken grupp av studenter som gjort varje labb-lösning
array [Solutions] of set of int: solution_group;

% Antal labbhandledare
int: teachers;

% Antal redovisningstider per handledare
int: timeslots;


%%%%%%%%%%%%%%%%%
% HJÄLPVARIABLER
%%%%%%%%%%%%%%%%%

% Mängden av alla labb-lösningar som ska redovisas
set of int: Solutions = 1..solutions; % {1,2,3,4}

% Mängd med samtliga studenter som ska redovisa något
set of int: Students = array_union(solution_group);

% Mängd med samtliga labbar som lösts
set of int: Labs = {lab | lab in solution_lab}; % {1,2}

% Mängden av alla labbhandledare
set of int: Teachers = 1..teachers;

% Mängden av alla redovisningstider
set of int: Timeslots = 1..timeslots;

% Hur många studenter som vill redovisa varje labb
array [Labs] of int: student_lab_count =  % [4,5]
   [sum (sol in Solutions where solution_lab[sol] = l) (card(solution_group[sol])) | l in Labs];

% Hur många redovisningar det borde bli per labb
array [Labs] of int: presentations_per_lab = [student_lab_count[l] div 2 | l in Labs]; % [2,2]

% Booleska värden för om en labb har udda antal studenter eller inte
array [Labs] of bool: lab_has_odd_count = [student_lab_count[l] mod 2 != 0 | l in Labs];

% Hur många redovisningar det borde bli totalt
int: presentations = sum(presentations_per_lab); % 4

% Mängden av alla redovisningar
set of int: Presentations = 1..presentations; % {1,2,3,4}

% Funktion för att beräkna vilken presentation from ska hantera vilken lab
function array[int] of Labs: compute_presentations(set of Labs: labs) =
          if card(labs) == 0 then
             []
          else
             let {
               int: next_lab = min(labs),               
               array[int] of Labs: result = [next_lab | i in 1..presentations_per_lab[next_lab]] ++ 
                                            compute_presentations(labs diff {next_lab})
             } in
             result
          endif;

% För varje redovisningsgrupp, vilken labb som ska redovisas
array [Presentations] of Labs: presentation_lab = compute_presentations(Labs);

% Vilka redovisningar som är för en lab
array [Labs] of set of Presentations: lab_presentations = [{p | p in Presentations where presentation_lab[p] == lab} 
                                                          | lab in Labs];

% Vilka lösningar som är för en lab
array [Labs] of set of Solutions: lab_solutions = [{p | p in Solutions where solution_lab[p] == lab} 
                                                          | lab in Labs];


%%%%%%%%%%%%%%%%%%%%
% LÖSNINGSVARIABLER
%%%%%%%%%%%%%%%%%%%%

% För varje redovisningsgrupp, vilka studenter som ingår i den
array [Presentations] of var set of Students: presentation_group;

% För varje redovisningsgrupp, vilken handledare som redovisningen är för
array [Presentations] of var Teachers: presentation_teacher;

% För varje redovisningsgrupp, vilken tid redovisningen är
array [Presentations] of var Timeslots: presentation_time;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VILLKOR FÖR ATT LÖSA PROBLEMET HÄR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constraint forall (g in presentation_group) (
  card(g) == 2 \/ card(g) == 3
);

%%%%
% Varje student ska få redovisa exakt de labbar 
% han eller hon löst.
%%%%

constraint forall (l in Labs)
  (array_union([solution_group[s] | s in Solutions where solution_lab[s] = l]) == 
   array_union([presentation_group[p] | p in Presentations where presentation_lab[p] = l])
  );

%%%%
% och inte behöva redovisa någon labb mer än en 
% gång.
%%%%
constraint forall (i,j in Presentations where
  (presentation_lab[i] == presentation_lab[j] /\ i != j))
  (presentation_group[i] intersect presentation_group[j] == {});

%%%%
% Varje lösningsgrupp ska bara ha en eller noll personer gemensamt 
% med varje presentationsgrupp där det handlar om samma laboration.
%%%%

constraint forall (s in Solutions)                       
  (forall (p in lab_presentations[solution_lab[s]])
    (card(presentation_group[p] intersect solution_group[s]) <= 1)  
  );                                                                


%%%%
% Varje par presentationer som har samma lärare men inte är samma 
% presentation ska inte utföras på samma tid.
%%%%
constraint forall (i,j in Presentations where 
  (presentation_teacher[i] == presentation_teacher[j] /\ i != j))
  (presentation_time[i] != presentation_time[j]);

%%%%
% Varje par presentationer som utförs på samma tid men inte är 
% samma presentation ska inte ha några elever gemensamma.
%%%%
constraint forall (i,j in Presentations where 
  (presentation_time[i] == presentation_time[j] /\ i != j)) 
  (presentation_group[i] intersect presentation_group[j] == {});



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SÖKSPECIFIKATION FÖR ATT LÖSA PROBLEMET
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Man kan om man vill speca sökningen mer detaljerat, t.ex. vilken 
% sökstrategi som ska användas.
solve satisfy;

%%%%%%%%%%%%%%%%%%%%%%
% UTSKRIFT AV LÖSNING
%%%%%%%%%%%%%%%%%%%%%%

output
 [show(presentations), " redovisningar:\n",
	"    Tid  Handl.  Labb  Studenter\n"] ++
 [ show_int(2, p) ++ ": " ++
   show_int(-3, presentation_time[p]) ++ "  " ++
   show_int(-6, presentation_teacher[p]) ++ "  " ++
   show_int(-4, presentation_lab[p]) ++ "  " ++
   show(presentation_group[p]) ++ "\n"
     | p in Presentations ];