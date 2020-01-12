:- consult('grammar').
:-op(500,yfx,#).


%% With provided #s modifier, predicate will create lexems in both states: plural and singular.
gen_s:-
	% connect all locations with prpoper prepositions
	retractall(lex(_,loc_prep,_)),
	findall([L,P],location_preposition(L,P),R),
	foreach(
		member([A,B],R),
		connect_loc_prep(A,B)
	),
	writeln('start story'),
	story(Story,[]),
	writeln('end story'),
	process_story(Story,ProcessedStory),
	foreach(member(X,ProcessedStory),(
		write(X),
		write(' ')
	)).



process_story([],[]).
process_story(['.',Word|Story],['.',Capitilized|Other]):-
	fist_to_upper(Word,Capitilized),
	process_story(Story, Other).
process_story([Word|Story],[Word|Result]):-
	process_story(Story, Result).

fist_to_upper(String, Capitilized):-
	string_chars(String, [FirstChar|Last]),
	upcase_atom(FirstChar, Uppercased),
	string_chars(Capitilized,[Uppercased|Last]).
	





add_lexem(Lexem#s,Term):-
	atom_concat(Lexem,'s',SingularLexem),
	SingularTerm =.. [lex, SingularLexem,Term,s],
	PluralTerm =.. [lex, Lexem,Term,p],
	assertz(PluralTerm),
	assertz(SingularTerm).

add_lexem(Lexem,Term):-
	assertz(lex(Lexem,Term,s)).
