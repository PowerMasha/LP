female('Мария Алексеева').
male('Алексей Алексеев').
female('Любовь Рябова').
female('Раиса Павлова').
male('Алексей Алексеев').
female('Антонина Куликова').
male('Алексей Алексеев').
female('Ева Алексеева').
female('Настя Ковалева').
male('Борис Рябов').
female('Мария Астафьева').
female('Людмила Павлова').
male('Анатолий Кузьменко').
male('Андрей Кузьменко').
male('Дима Кузьменко').
male('Алексей Павлов').
female('Зинаида Мельникова').
male('Борис Алексеев').
female('Надежда Панфилова').
male('Федор Куликов').
male('Владислав Рябов').
female('Галина Петрова').
female('Светлана Корж').
parents('Алексей Алексеев', 'Алексей Алексеев', 'Любовь Рябова').
parents('Мария Алексеева', 'Алексей Алексеев', 'Любовь Рябова').
parents('Алексей Алексеев', 'Алексей Алексеев', 'Раиса Павлова').
parents('Любовь Рябова', 'Борис Рябов', 'Антонина Куликова').
parents('Ева Алексеева', 'Алексей Алексеев', 'Настя Ковалева').
parents('Раиса Павлова', 'Алексей Павлов', 'Мария Астафьева').
parents('Людмила Павлова', 'Алексей Павлов', 'Мария Астафьева').
parents('Андрей Кузьменко', 'Анатолий Кузьменко', 'Людмила Павлова').
parents('Дима Кузьменко', 'Анатолий Кузьменко', 'Людмила Павлова').
parents('Алексей Алексеев', 'Борис Алексеев', 'Зинаида Мельникова').
parents('Антонина Куликова', 'Федор Куликов', 'Надежда Панфилова').
parents('Борис Рябов', 'Владислав Рябов', 'Галина Петрова').

father(X, Y) :-
    parents(X, Y, _).

mother(X, Y) :-
    parents(X, _, Y).

child(X, Y) :- 
    parents(X, Z, Y);
    parents(X, Y, Z).

brother(X, Y) :- % брат
    parents(X, F, M),
    parents(Y, F, M),
    X\=Y, male(Y).

sister(X, Y) :- % сестра
    parents(X, F, M),
    parents(Y, F, M),
    X\=Y, female(Y).

dever(Y,X) :- 
	parents(_,A,X),
	brother(A,Y).
	

% степень родства

relative(child, Child, Parent) :-
    parents(Child, _, Parent);
    parents(Child, Parent, _).

relative(sister, Sister, Brother) :-
    sister(Sister, Brother).

relative(brother, Brother, Sister) :-
    brother(Brother, Sister).

relative(father, Father, Child) :-
    parents(Child, Father, _).

relative(mother, Mother, Child) :-
    parents(Child, _, Mother).

relative(husband, Husband, Wife) :-
    parents(_, Husband, Wife), !.

relative(wife, Wife, Husband) :-
    parents(_, Husband, Wife), !.

relative(Way, First, Last) :- 
    dpath(First, Last, Way), !.

chain_of_relation(X):-
      member(X, [child, father, mother, sister, 
      brother, husband, wife]).

print_ans([]).
print_ans([H|Tail]):-
    print_ans(Tail),
    write(H),  write(' - ').

next(Curr, HasBeen, Y, Rel) :-
   (relative(Rel, Curr, Y);
   relative(Rel, Y, Curr)),
   not(member(Y, HasBeen)).

dFS(X, X, _, _).

dFS(Curr, Last, T, [Rev|Tl]) :- 
    next(Curr, T, Next, Rev),
    dFS(Next, Last, [Curr|T], Tl),
    !.

dpath(First, Last, Way) :- 
    dFS(First, Last, [], RevWay),
    reverse(RevWay, Way),
	print_ans(Way),
	!.
