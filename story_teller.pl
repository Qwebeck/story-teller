:- consult('grammar').
:- consult('utils').


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
	process_story(Story,ProcessedStory),
	tell_story(ProcessedStory).






