%Merve Cerit - 2012402015 - Project1
%We should define dynamic predicates for the purposes of asserting and retracting later.

:-dynamic(student/2).
:-dynamic(available_slots/1).
:-dynamic(room_capacity/2).

%Queries

%3.1 clear_knowledge_base.

countstudents(X) :-
	findall(N, student(N,_), Ns),
	length(Ns, X).
	%counts students by their id. And reports it.
countavailableslots(X):-
	findall(N,available_slots(N),Ns),
	length(Ns,X).
	 %counts all available slots. And reports it.
countroomcapacity(X):-
	findall(N,room_capacity(N,_),Ns),
	length(Ns,X).
	%counts all room_capacity predicates by looking at the room. And reports it.

clear_knowledge_base:-
	countstudents(X1),writeln('student/2':X1),
	countavailableslots(X2),
	writeln('available_slots/1':X2),
	countroomcapacity(X3),
	writeln('room_capacity/2':X3),
	retractall(student(_,_)),
	retractall(available_slots(_)),
	retractall(room_capacity(_,_)).





%3.2 all_students(-StudentList).
	%gives the all student ids in a list.

all_students(StudentList):-findall(X,student(X,_),StudentList).





%3.3 all_courses(-CourseList).
	%gives the all courses in a list.(no repetition)
all_courses(CourseList):-findall(Y,student(_,Y),CourseList1),
	flatten_the_list(CourseList1,CourseList),!.
%first we get the lists of the courses in a list as CourseList1.Then by flatten function, made it 1 list.

flatten_the_list([H1|T1],[H1|T1]) :- \+ is_list(H1).
%checks if there is list of lists.(stopping condition-1)

flatten_the_list([],[]). %stopping condition-0

flatten_the_list([H|T],List):-flatten_the_list(T,List1), append_uniquely(H,List1,List).
%recursively calls flatten and appends opened lists at each stage.

%predicates below doe the appending, but append_uniquely makes sure that is no repetition in the list.

append_uniquely([],L,L).

append_uniquely([H|L1],L2,[H|L3]):-
	member(H,L2)->remove_element(H,L2,L4),append_uniquely(L1,L4,L3);
	append_uniquely(L1,L2,L3).

remove_element(X,[X|Tail],Tail).

remove_element(X,[Y|Tail],[Y|Tail1]):-remove_element(X,Tail,Tail1).










%3.4 student_count(+CourseID,-StudentCount).

%taking the list of all courses taken as well as the CourseID is the point here. student_count_v2 is doing the real job.

student_count(CourseID,StudentCount):-
	findall(X,student(_,X),List),
	student_count_v2(CourseID,List,0,StudentCount).

student_count_v2(_,[],StudentCount,StudentCount).

student_count_v2(CourseID,[H|T],Counter,StudentCount):-
	member(CourseID,H)->(Count is Counter+1,student_count_v2(CourseID,T,Count,StudentCount));
	(Count is Counter,student_count_v2(CourseID,T,Count,StudentCount)),!.









%3.5 common_students(+CourseID1,+CourseID2,-StudentCount).

/*this predicate is almost the same with the student_count, only difference is having two checkpoints related to the membership. If and only if
both of the courses are the members of the list, then counter is incremented. */

common_students(CourseID1,CourseID2,StudentCount):-
	findall(X,student(_,X),List),
	common_students_v2(CourseID1,CourseID2,List,0,StudentCount).

common_students_v2(_,_,[],StudentCount,StudentCount).

common_students_v2(CourseID1,CourseID2,[H|T],Counter,StudentCount):-
	(member(CourseID1,H),member(CourseID2,H))->(Count is Counter+1,common_students_v2(CourseID1,CourseID2,T,Count,StudentCount));
	(Count is Counter,common_students_v2(CourseID1,CourseID2,T,Count,StudentCount)),!.








%3.6 final_plan(-FinalPlan).

all_rooms(RoomList):-findall(X,room_capacity(X,_),RoomList). % this predicate gives us all room names.

%for each course we should try tuples of room and slots.
units(Room,Slot):-
	all_rooms(RoomList),
	available_slots(Slots),
	member(Room,RoomList),
	member(Slot,Slots).

%we add the units to the plan and we make sure that there is no violation of error rules.
check_finalplan([],[]).
check_finalplan([H|T], Plan):-
	check_finalplan(T,Subplan),
	units(X,Y),
	append([[H,X,Y]],Subplan,Plan),
	errors_for_plan(Plan,ErrorCount),
	ErrorCount=0,
	error_type3(Plan,ErrorCount3),
	ErrorCount3=0.

final_plan(FinalPlan):- all_courses(CourseList),check_finalplan(CourseList,FinalPlan).

%this predicate makes sure that there is no final assignment such that room and slot is same.
error_type3([],0).
error_type3([H|T],Error):-
	error_type3(T,SubError1),
	error_type3_check(H,T,SubError2),
	Error is SubError1+SubError2.

error_type3_check(_,[],0).
error_type3_check([_|[Room|[Slot|_]]],[[_|[Room1|[Slot1|_]]]|Tail],SubError2):-
	error_type3_check([_|[Room|[Slot|_]]],Tail,SubError3),
	Room=Room1,
	Slot=Slot1,
	SubError2 is 1+SubError3.
error_type3_check([_|[Room|[Slot|_]]],[[_|[Room1|[Slot1|_]]]|Tail],SubError3):-
	error_type3_check([_|[Room|[Slot|_]]],Tail,SubError3),
	not(Slot=Slot1),
	Room=Room1.
error_type3_check([_|[Room|[Slot|_]]],[[_|[Room1|[Slot1|_]]]|Tail],SubError3):-
	error_type3_check([_|[Room|[Slot|_]]],Tail,SubError3),
	not(Slot=Slot1),
	not(Room=Room1).
error_type3_check([_|[Room|[Slot|_]]],[[_|[Room1|[Slot1|_]]]|Tail],SubError3):-
	error_type3_check([_|[Room|[Slot|_]]],Tail,SubError3),
	Slot=Slot1,
	not(Room=Room1).

%3.7 errors_for_plan(+FinalPlan,-ErrorCount).

%the predicate below is the main predicate. I divide this into two for the two types of errors,
%then I am summing them up.

errors_for_plan(FinalPlan,ErrorCount):-  
	error_type1(FinalPlan,ErrorCount1),error_type2(FinalPlan,ErrorCount2),
	ErrorCount is ErrorCount1+ErrorCount2,!.

%error_type1 represents same slot error. I am recursively calling it and error_type1_check. 
%It utilizes the common_students predicate.

error_type1([],0).
error_type1([H|T],Error):- 
	error_type1(T,SubError1),
	error_type1_check(H,T,SubError2),
	Error is SubError1+SubError2.

error_type1_check(_,[],0).
error_type1_check([Course|[Room|[Slot|_]]],[[Course1|[_|[Slot1|_]]]|Tail],SubError2):-
	error_type1_check([Course|[Room|[Slot|_]]],Tail,SubError3),
	Slot=Slot1,
	common_students(Course,Course1,SubError4),
	SubError2 is SubError4+SubError3.
error_type1_check([Course|[Room|[Slot|_]]],[[_|[_|[Slot1|_]]]|Tail],SubError3):-
	error_type1_check([Course|[Room|[Slot|_]]],Tail,SubError3),
	not(Slot=Slot1).

%error_type2 finds capacity error.For finding the excess students, I wrote another predicate. 
%Because it is just looking elements one by one, there is no second function as above function error_type1_check.

error_type2([],0).
error_type2([[Course|[Room|[_|_]]]|Tail],Error):-
	error_type2(Tail,SubError1),
	excess_students(Course,Room,SubError2),
	Error is SubError1+SubError2.
excess_students(Course,Room,Error):-
	student_count(Course,StudentCount),
	room_capacity(Room,Capacity),
	StudentCount>Capacity,
	Error is StudentCount-Capacity.
excess_students(Course,Room,Error):-
	student_count(Course,StudentCount),
	room_capacity(Room,Capacity),
	not(StudentCount>Capacity),
	Error is 0.





