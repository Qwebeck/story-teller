:- consult('lexic').
:- use_module(library(aggregate)).

story --> {random_grammar_clause(2,introduction,Introduction)}, 
	   Introduction.
%story --> s(introduction,Time),['.'],
%	  s(hero_acts,Time),['.'],
%	  s(hero_acts,Time).

%% It was a .... in ....
%% It is a ... in ....
%% Hero ... made ..... -> caused.

introduction(place_desription,Time) --> {rand_word([times],0,Time)},
			       place_p(Time).

introduction(action,Time) --> {rand_word([times],0,Time)},
			      s(hero_acts, Time).



s(hero_acts,Time) --> {random_grammar_clause(1,hero_p,HeroPhrase)},
		      HeroPhrase,
		      action(Time). 

%%'It is a sunny day in the Galactic Hall. Tired, Anakin learning to play the guitar'.
%% 'Exited with new toy, Anakin learing to play the guitar'.
% Time is a time form of action: continous, past perfect, present perfect.
action(Time) --> vp(Time),
		 np.

%vp(present) --> v(infinitive).

%vp(continous) --> v(continous), 
%		  prep, 
%		  n.

vp(past) --> sc(past),
	     v(continous),
	     prep,
	     n.

hero_p(simple) --> hero.

hero_p(extended) --> participle_p,
		     hero.

place_p(Time) --> ['It'], 
		  sc(Time),
		  article, 
		  adj(wapp),
		  period,
		  {random_grammar_clause(1,place_ph,PlacePhrase)},
		  PlacePhrase.

place_ph(extended) --> prep(prp),
	               adj(papp),
             	       place.

place_ph(simple) --> prep(prp),
	     	     place.

participle_p --> participle,
		 prep,
		 np,
		 [','].

np --> article,
       n.


prep --> [Word],{rand_word([prep,_],0,Word)}.

prep(Target) --> [Word],{rand_word([prep,Target],0,Word)}.

n --> [Word],{rand_word([n, _],0,Word)}.

article --> [Word],{rand_word([article],0,Word)}.

v(GrammarForm) --> {rand_lexem([v,GrammarForm,_], 0, lex(Word,_,_,PrepositionList)),
		    connect_verb_prep(Word,PrepositionList,Result)},
		    Result.

hero --> [Word],{rand_word([hero],0,Word)}.

participle --> [Word],{rand_word([participle,_],0,Word)}.

adj(Target) --> [Word],{rand_word([adj,Target],0,Word)}.

sc(Time) --> [Word],{rand_word([sc,Time],0,Word)}.

period --> [Word],{rand_word([period],0,Word)}.

place --> [Word],{rand_word([place],0,Word)}.



insert(Element, 0, List, [Element|List]).
insert(Element, I, [H|T], [H|NT]) :-
    I > 0,
    NI is I - 1,
    insert(Element, NI, T, NT).
 


%% Wrapper around get_rand_lexem, that takes a list of ignore parametrs, as argument.
rand_word(IgnoreParamsList, GoalPos, Result):-   	
	insert(_,GoalPos,IgnoreParamsList,NewPred),
    	Template =.. [lex|NewPred],
	bagof(Template,IgnoreParamsList^Template, PossibleLexems),
	length(PossibleLexems, L),
	random(0, L, I),
	nth0(I, PossibleLexems, Lexem),
	Lexem =.. [lex|Params],
	nth0(GoalPos,Params,Result).

rand_lexem(IgnoreParamsList, GoalPos, Result):-   	
	insert(_,GoalPos,IgnoreParamsList,NewPred),
    	Template =.. [lex|NewPred],
	bagof(Template,IgnoreParamsList^Template, PossibleLexems),
	length(PossibleLexems, L),
	random(0, L, I),
	nth0(I, PossibleLexems, Result).


connect_verb_prep(Verb,PrepList,[Verb,Prep]):-
	length(PrepList,L),
	random(0,L,I),
	nth0(I,PrepList,Prep).


%% Returns random grammar clause
%% ArgCount - number of arguments, grammmar clause accepts (without undeneath differential list)
%% Clause - name of grammar clause 
random_grammar_clause(ArgCount, Predicate, Result):-
	C is ArgCount + 2,
	length(Args, C),
	GrammarC =.. [Predicate|Args],
	aggregate(count,Args^GrammarC,NumberOfPossibilities),
	random_between(1,NumberOfPossibilities, I),	
	nth_clause(GrammarC, I, R), clause(ClauseWithDiffList, _, R),
	remove_diff_list_from_clause(ClauseWithDiffList,Result).


remove_diff_list_from_clause(ClauseWithDiff, Result):-
	ClauseWithDiff =.. [Predicate|Params],
	length(DiffList, 2),
	append(ParamsWithoutDiffList, DiffList, Params),
	Result =.. [Predicate|ParamsWithoutDiffList].

