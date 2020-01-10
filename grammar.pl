:- consult('lexic').

story --> s(introduction,Time),['.'],s(hero_acts,Time).

s(introduction,Time) --> {rand_lexem([times],0,Time)}, place_p(Time).
s(hero_acts,Time) --> hero_p(extended),action(Time). 

%%'It is a sunny day in the Galactic Hall. Tired, Anakin learning to play the guitar'.
%% 'Exited with new toy, Anakin learing to play the guitar'.
% Time is a time form of action: continous, past perfect, present perfect.
action(Time) --> vp(Time), np.
vp(present) --> v(infinitive).
vp(continous) --> v(continous), prep, v(infinitive).
vp(past) --> np.
hero_p(simple) --> hero.
hero_p(extended) --> participle_p, hero.
place_p(Time) --> ['It'], sc(Time), article, adj(wapp), period, place_ph.
place_ph --> prep(papp), adj(papp), place.
place_ph --> prep(papp), place.
participle_p --> participle ,prep, np, [','].
np --> article, n.




prep --> [Word],{rand_lexem([prep],0,Word)}.
prep(Target) --> [Word],{rand_lexem([prep,Target],0,Word)}.
n --> [Word],{rand_lexem([n, _],0,Word)}.
article --> [Word],{rand_lexem([article],0,Word)}.
v(GrammarForm) --> [Word],{rand_lexem([v,GrammarForm], 0, Word)}.
hero --> [Word],{rand_lexem([hero],0,Word)}.
participle --> [Word],{rand_lexem([participle,_],0,Word)}.
adj(Target) --> [Word],{rand_lexem([adj,Target],0,Word)}.
sc(Time) --> [Word],{rand_lexem([sc,Time],0,Word)}.
period --> [Word],{rand_lexem([period],0,Word)}.
place --> [Word],{rand_lexem([place],0,Word)}.



insert(Element, 0, Lista, [Element|Lista]).
insert(Element, I, [H|T], [H|NT]) :-
    I > 0,
    NI is I - 1,
    insert(Element, NI, T, NT).
 


%% Wrapper around get_rand_lexem, that takes a list of ignore parametrs, as argument.
rand_lexem(IgnoreParamsList, GoalPos, Result):-   	
	insert(_,GoalPos,IgnoreParamsList,NewPred),
    	Template =.. [lex|NewPred],
	bagof(Template,IgnoreParamsList^Template, PossibleLexems),
	length(PossibleLexems, L),
	random(0, L, I),
	nth0(I, PossibleLexems, Lexem),
	Lexem =.. [lex|Params],
	nth0(GoalPos,Params,Result).
	



