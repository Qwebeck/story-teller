:- consult(lexic).
:- use_module(library(aggregate)).
:- (dynamic calls/1).

max_calls(4).

% Goals:
% Add relations between phrases.
% Add more events
% Add UI, that allow user to add his own clausules: name, phrase, ....
% For safety reasons, write a predicate, that connects all places with predicates
introduction_number(1).
event_number(2).

story -->
    { introduction_number(N),
      random_grammar_clause_v2(introduction, N, [_], Introduction)
    },
    Introduction,
    { writeln('Introduction End')
    }. 

%
% It was sunny outside, so his mood was good.
% It was rainy outside, so he was angry.
% It is .... outside, so he is ... .
% Hero was sitting in [rest place] and [doing some action]. He was [mood].
% Suddenly another hero comes in [hero location].
% [our hero], [other hero replic]. [our hero answer dependent on mood].
introduction(start_in_place) -->
    place_descr(GlobalMood, Location),
    hero_descr(GlobalMood, Location, Hero, ConcretePlace, HeroMood),
    { writeln([GlobalMood, Location, Hero, ConcretePlace, HeroMood]),
      event_number(N),
      random_grammar_clause_v2(event,
                               N,
                               
                               [ _,
                                 GlobalMood,
                                 Location,
                                 Hero,
                                 ConcretePlace,
                                 HeroMood
                               ],
                               Event),
      writeln(Event)
    },
    Event,
    { writeln('End event')
    }.

							
% Return hero name, and his concrete location
hero_descr(GlobalMood, Location, Hero,ConcretePlace, HeroMood) --> hero(Hero),
												   hero_action,
												   hero_location(Location,ConcretePlace),
												   ['.'],
												   ['he was'],
												   hero_mood(GlobalMood, HeroMood),
												   mood_reason(HeroMood),
												   ['.'].

event(other_hero,GlobalMood,_Location,Hero,ConcretePlace,HeroMood)-->
	['Suddenly'],
	hero_mood(GlobalMood,_AnotherHeroMood),
	hero(no_adj, AnotherHero),
	['comes in'],
	[ConcretePlace],
	[':'],
	['"'],
	hero_speaks(AnotherHero,Hero,Asks),
	ask_explanation(Asks,SubjectOfAsk),
	['"'],
	hero_reacts(HeroMood).
	
event(robbery,GlobalMood,Location,Hero,ConcretePlace,HeroMood)-->[robbery].



% Explains what  present someone presenting. Returns given present. 
ask_explanation(present,Present) --> [Phrase],
							 {
							   rand_word([present_phrase],0,Phrase)  
							 },
							 np(Present).

ask_explanation(_) --> [empty].

% Returns text of greeting, and ask of Hero.
hero_speaks(Hero, ToHero, Asks) -->  greeting(Hero),
									 [ToHero],
									 ['.'],
							     	 replic(replic,Asks).

% Takes as arguments Hero, and his mood
hero_reacts(HeroMood) --> {lex(HeroMood,mood,AnswerTone)},
						  replic(answer,AnswerTone).

% Return replic of hero as text, and Meaning of replic.
%replic(Hero,Asks)

% Return mood of hero
hero_mood(GlobalMood, HeroMood) --> mood(GlobalMood, HeroMood).

% Reason of mood.

% Return concrete location of hero
hero_location(Location,ConcretePlace) --> [Preposition],
										adj(papp),
										place(Location, ConcretePlace),
										{
											writeln(ConcretePlace),
											lex(ConcretePlace,loc_prep,ProperPrepositions),
											random_element(ProperPrepositions,Preposition)

										}.

% In case if location,doesn't connected with any prepositions and first predicate will crash
hero_location(Location,ConcretePlace) --> prep(prp),
										  adj(papp),
										  place(Location, ConcretePlace).

hero_action --> ['was'],
				v(continous),
				n.

% Return Mood, Location to introduction
place_descr(Mood, Location) --> weather(Mood),
								{(var(Mood)->Mood=good;
								 true)},
								[Preposition],
								adj(papp),
								place(Location),
								['.'],
								{	
									writeln(Location),
									lex(Location,loc_prep,ProperPrepositions),
									random_element(ProperPrepositions,Preposition),
									writeln(Preposition)
								}.

% Worksin case if location doesn't connect with any prepositions and first predicate will crash
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

% Describes what could be the reason of hero mood
mood_reason(HeroMood) --> [Word],{
							lex(HeroMood, mood, Intention),
							rand_word([reas_mood,Intention],0,Word)
							}.

% Returns target noun
np(Target) --> article,
		adj(tapp),
		n(Target).	
		

article --> [Word],{rand_word([article],0,Word)}.
adj(Target) --> [Word],{rand_word([adj,Target,_],0,Word)}.
% Returns random weather and mood associated with it
adj(Target,Mood) --> [Word],{rand_lexem([adj,Target,_],0,lex(Word,_,_,Mood))}.
n --> [Word],{rand_word([n, _],0,Word)}.
% Returns target, that was presented
n(Target) --> [Word],{rand_word([n, _],0,Word),Target=Word}.
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

hero(no_adj, Hero) -->  [Hero],
						{rand_lexem([hero],0,lex(Hero,_))}.

hero(Hero) --> adj(happ),
			   [Hero],
			   {rand_lexem([hero],0,lex(Hero,_))}.

replic(Type,Asks) --> [Word],
				{
					rand_lexem([Type,Asks], 0, lex(Word,_,Asks))
				}.			

% Return mood 
mood(GMood,HeroMood) --> [HeroMood],{rand_word([mood,GMood],0,HeroMood)}.
% Return greeting, that characterical for Hero
greeting(Hero) --> [Greeting],{rand_word([greeting, Hero],0,Greeting)}.





random_element(List,Element):-
	length(List,L),
	random(0, L, I),
	nth0(I,List,Element).


% insert(Element,0,L,[Element|L]). % ok
% insert(Element,Pos,[E|L],[E|ZL]):- % you forgot to cons back E
%     Pos1 is Pos-1,
%     insert(Element,Pos1,L,ZL). % done, append is useless
%     %append(E,ZL1,ZL).


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

% Don't use aggregate, because prolog goes through all possibilities, while counting with aggregate. 
% The reason is - is that aggregate - is an implemantation of bagof, an bagof looks untill he won't fail.
random_grammar_clause_v2(Predicate, NumberOfPossibilities, Args, Result):-
	length(Pl, 2),
	append(Args,Pl,Arglist),
	GrammarC =.. [Predicate|Arglist],
	% aggregate(count,Args^GrammarC,NumberOfPossibilities),
	random_between(1,NumberOfPossibilities, I),	
	nth_clause(GrammarC, I, R), clause(ClauseWithDiffList, _, R),
	remove_diff_list_from_clause(ClauseWithDiffList, Args,Result).




%% Returns random grammar clause
%% ArgCount - number of arguments, grammmar clause accepts (without undeneath differential list)
%% Clause - name of grammar clause 
random_grammar_clause(ArgCount, Predicate, NumberOfPredicates, Result):-
	C is ArgCount + 2,
	length(Args, C),
	GrammarC =.. [Predicate|Args],
	% Don't use it, because prolog goes through all possibilities, while counting with aggregate. 
	% The reason is - is that aggregate - is an implemantation of bagof, an bagof looks untill he won't fail.
	% aggregate(count,Args^GrammarC,NumberOfPossibilities),
	% writeln(NumberOfPossibilities),
	random_between(1,NumberOfPredicates, I),	
	writeln(I),
	nth_clause(GrammarC, I, R),writeln([GrammarC]),clause(ClauseWithDiffList, _, R),
	writeln('Should enter remove'),
	remove_diff_list_from_clause(ClauseWithDiffList,Result).

remove_diff_list_from_clause(ClauseWithDiff, Result):-
	writeln('Enter remove'),
	ClauseWithDiff =.. [Predicate|Params],
	writeln(['Here',ParamsWithoutDiffList]),	
	length(DiffList, 2),
	append(ParamsWithoutDiffList, DiffList, Params),
	writeln(ParamsWithoutDiffList),	
	Result =.. [Predicate|ParamsWithoutDiffList].

remove_diff_list_from_clause(ClauseWithDiff, Args,Result):-
	ClauseWithDiff =.. [Predicate|Params],
	length(DiffList, 2),
	append(ParamsWithoutDiffList, DiffList, Params),
	ParamsWithoutDiffList = Args,
	Result =.. [Predicate|Args].

