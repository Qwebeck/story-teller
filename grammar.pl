:- consult('lexic').
:- use_module(library(aggregate)).
:- dynamic calls/1.

max_calls(4).


story --> {
			random_grammar_clause(1,introduction,Introduction)
		  }, 
			Introduction. 

%
% It was sunny outside, so his mood was good.
% It was rainy outside, so he was angry.
% It is .... outside, so he is ... .
% Hero was sitting in [rest place] and [doing some action]. He was [mood].
%
introduction(start_in_place) --> place_descr(GlobalMood,Location), 
								hero_descr(GlobalMood, Location, Hero,ConcretePlace, HeroMood),
								{writeln([GlobalMood, Location, Hero,ConcretePlace, HeroMood])}.

% Return hero name, and his concrete location
hero_descr(GlobalMood, Location, Hero,ConcretePlace, HeroMood) --> hero(Hero),
												   hero_action,
												   hero_location(Location,ConcretePlace),
												   hero_mood(GlobalMood, HeroMood).

% Return mood of hero
hero_mood(GlobalMood, HeroMood) --> ['He was'],
									mood(GlobalMood, HeroMood),
									[.].


% Return concrete location of hero
hero_location(Location,ConcretePlace) --> prep(prp),
										adj(papp),
										place(Location, ConcretePlace),
										[.].


hero_action --> ['was'],
				v(continous),
				n,
				[.].

% Return Mood, Location to introduction
place_descr(Mood, Location) --> weather(Mood),
								{(var(Mood)->Mood=good;
								 true)},
								prep(prp),
								adj(papp),
								place(Location),
								['.'].
			 

% Return type of weather outside. 
weather(Type) --> ['It is a'],
			      adj(wapp,Type),
			      period.


article --> [Word],{rand_word([article],0,Word)}.
adj(Target) --> [Word],{rand_word([adj,Target,_],0,Word)}.
% Returns random weather and mood associated with it
adj(Target,Mood) --> [Word],{rand_lexem([adj,Target,_],0,lex(Word,_,_,Mood))}.
n --> [Word],{rand_word([n, _],0,Word)}.
prep --> [Word],{rand_word([prep,_],0,Word)}.
prep(Target) --> [Word],{rand_word([prep,Target],0,Word)}.
period --> [Word],{rand_word([period],0,Word)}.

% Location in place is the same is place.
place(Location) --> [Word],{rand_word([place,_],0,Word),Location = Word}.
% Returns possible location of hero in place he now.
place(GlobalLocation,ConcretePlace) --> [ConcretePlace],
										{
											rand_lexem([GlobalLocation,place],2,lex(_,_,Places)),
											random_element(Places,ConcretePlace)
										}.

v(GrammarForm) --> {rand_lexem([v,GrammarForm,_,_], 0, lex(Word,_,_,PrepositionList,_)),
		    connect_verb_prep(Word,PrepositionList,Result)},
		    Result.
v(GrammarForm,Intention) --> {rand_lexem([v,GrammarForm,_,Intention], 0, lex(Word,_,_,PrepositionList,_)),
		connect_verb_prep(Word,PrepositionList,Result)},
		Result.			

hero(Hero) --> adj(happ),
		[Hero],
		{rand_lexem([hero],0,lex(Hero,_))}.

% Return mood 
mood(GMood,HeroMood) --> [HeroMood],{rand_word([mood,GMood],0,HeroMood)}.


random_element(List,Element):-
	length(List,L),
	random(0, L, I),
	nth0(I,List,Element).
% hero --> [Word],{rand_word([hero],0,Word)}.












% 
% exclam --> [Word],{rand_word([exclam],0,Word)}.

% participle --> [Word],{rand_word([participle,_],0,Word)}.




% sc(Time) --> [Word],{rand_word([sc,Time],0,Word)}.





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

% next_statement(Condition, Predicate, ArgCount, SentenceIfFalse, Result):-
% 	(Condition ->(random_grammar_clause(ArgCount,Predicate,Result));
% 	(Result = SentenceIfFalse)
% 	).

% introduction_condition:-
% 	retract(calls(N)),
% 	max_calls(M),
% 	N<M,
% 	IncN is N + 1,
% 	assertz(calls(IncN)).


% repeat(Predicate, Args,Calls, MaxCalls, Result):-
% 	Calls<MaxCalls,
% 	C is Calls + 1,
% 	repeat(Predicate, Args,C,MaxCalls,MidRes),
% 	append([Predicate|Args],[X,[]],P),
% 	Pred =.. P,
% 	Pred,
% 	append(X, MidRes, Result),
% 	writeln(MidRes),
% 	writeln(X)
% 	.

% repeat(Predicate, Args,Calls,MaxCalls,X):-
% 	writeln(Calls),
% 	Calls = MaxCalls,
% 	append([Predicate|Args],[X,[]],P),
% 	Pred =.. P,
% 	Pred.







% place_p(Time) --> ['It'], 
% 		  sc(Time),
% 		  article, 
% 		  adj(wapp),
% 		  period,
% 		  {random_grammar_clause(1,place_ph,PlacePhrase)},
% 		  PlacePhrase.

% place_ph(extended) --> prep(prp),
% 	               adj(papp),
%              	       place.

% place_ph(simple) --> prep(prp),
% 	     	     place.


% introduction(start_from_action)-->[true].


% introducr,Time) --> {rand_word([times],0,Time)},
% 			place_p(Time),
% 			['.'],
% 			chapter.
	 
% introduction(action,Time) --> {rand_word([times],0,Time)},
% 		   s(hero_acts, Time),
% 		   chapter.


% conclusion --> ['and the day end'].

% chapter--> s(event,bad), s(event,bad), s(event,bad), conclusion.



			


% s(event,Intention) --> exclam, 
% 					   action(continous,Intention).

% s(empty,_) --> ['The end'].

% s(hero_acts,Time) --> {random_grammar_clause(2,hero_p,HeroPhrase)},
% 		      HeroPhrase,
% 			  action(Time),
% 			  ['.']. 


% action(Time) --> vp(Time),
% 		 		 np.

% action(Time,Intention) -->  hero_p(with_adj,Intention),
% 							 vp(Time,Intention).

% vp(present) --> sc(present),v(infinitive).

% vp(continous) --> sc(continous), v(continous).

% vp(past) --> sc(past),
% 	     	 v(continous).

% vp(Time,Intention) -->  sc(Time),v(Time,Intention), n.

% hero_p(with_adj,Intention) --> 
% 							  adj(happ,Intention),
% 					          hero.

% hero_p(simple,_) --> hero.

% hero_p(extended,_) --> participle_p,
% 		     		 		   hero.





% participle_p --> participle,
% 		 prep,
% 		 np,
% 		 [','].

% np --> article,
%        n.