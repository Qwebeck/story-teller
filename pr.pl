:- consult('grammar').
:-op(500,yfx,#).

%% Create grammar for history.
%% Allow to add new values to lexems
%% History features:
%% 1. One character during the story.
%% 2. Random stories every time.

%% With provided #s modifier, predicate will create lexems in both states: plural and singular.
gen_s:-
	% connect all locations with prpoper prepositions
	retractall(lex(_,loc_prep,_)),
	findall([L,P],location_preposition(L,P),R),
	foreach(
		member([A,B],R),
		connect_loc_prep(A,B)
	),

	story(Story,[]),
	foreach(member(X,Story),(
		write(X),
		write(' ')
	)).

add_lexem(Lexem#s,Term):-
	atom_concat(Lexem,'s',SingularLexem),
	SingularTerm =.. [lex, SingularLexem,Term,s],
	PluralTerm =.. [lex, Lexem,Term,p],
	assertz(PluralTerm),
	assertz(SingularTerm).

add_lexem(Lexem,Term):-
	assertz(lex(Lexem,Term,s)).
