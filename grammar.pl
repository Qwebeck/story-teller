:- consult('lexic').

:- use_module(library(aggregate)).
:- (dynamic calls/1).

% Number of grammar clause. Used to randomly choose between tham, 
% with predicate random_grammar_clause_v2/4
introduction_number(1).
event_number(1).

story -->
    { introduction_number(N),
      random_grammar_clause_v2(introduction, N, [_], Introduction)
    },
	Introduction.
	% {writeln('End Introduction')}. 

introduction(start_in_place) -->
    place_descr(GlobalMood, Location),
    hero_descr(GlobalMood, Location, Hero, ConcretePlace, HeroMood),
    {
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
                               Event)
	},
	Event.							
% Return hero name, and his concrete location
hero_descr(GlobalMood, Location, Hero,ConcretePlace, HeroMood) --> hero(Hero),
												   hero_action(GlobalMood),
												   hero_location(Location,ConcretePlace),
												   ['.'],
												   ['he was'],
												   mood(GlobalMood, HeroMood),
												   mood_reason(HeroMood),
												   ['.'].

% Event of other hero coming to main hero
event(other_hero,GlobalMood,Location,Hero,ConcretePlace,HeroMood)-->
	% {writeln(['Place: ',ConcretePlace]),
	{
	 ConcretePlace = [Preposition,Place]
	%  writeln(['Enter event with',GlobalMood,Hero,ConcretePlace,HeroMood])
	},
	['Suddenly'],
	mood(GlobalMood,AnotherHeroMood),
	hero(no_adj, Hero,AnotherHero),
	['comes '],
	[Preposition,Place],
	[':'],
	%br- indicates, that prolog should start here a new line  
	[br],
	['"'],
	hero_speaks(AnotherHero,Hero,Asks),
	ask_explanation(Asks,SubjectOfAsk,Hero,Location),
	['.'],
	[br],
	['"'],
	% {writeln(['Enter hero reacts 1:',HeroMood,Asks,SubjectOfAsk, AnotherHero, AnswerTone])},
	hero_reacts(HeroMood,Asks,SubjectOfAsk, AnotherHero, AnswerTone),
	['"-said'],
	[Hero],
	['.'],
	[br],
	{
		% writeln('Comes in block'),
		% answer of main hero can choose mood of other hero on negative.
		% but he can also stay in his normal mood
		lex(AnotherHeroMood,mood,AnotherHeroIntention),
		% writeln([AnotherHeroMood,AnotherHeroIntention]), 
		random_element([AnswerTone,AnotherHeroIntention],Tone),
		% Every reaction related to some subject of ask. 
		% map answer tone to possible reactions
		% writeln([Asks,Tone]),
		rand_word([reaction,Asks,Tone],0,Reaction)
		% writeln('Exit from block')
	},
	['"'],
	% {writeln(['Params',AnotherHeroMood,Reaction, SubjectOfAsk,Hero])},
	hero_reacts(AnotherHeroMood,Reaction, SubjectOfAsk,Hero,_),
	['"-answer'],
	[AnotherHero],
	{random_element([good,bad],Final)},
	[br],
	ending(Asks, Final, Hero,AnotherHero,Location).

ending(help, good,Hero,AnotherHero, Location)-->[Hero],['completed his promise and helped'],[AnotherHero,'to save',Location].
ending(help, bad,Hero,AnotherHero, Location)-->[Hero],['tried to help '],[AnotherHero,'to save ',Location, ' but he met with failure'].
ending(_, _,_,_, _) --> ['The end.'].
% Currently unsed event, but can be implemented in code.
%event(robbery,_GM,_Location,_Hero,_ConcretePlace,_HeroMood)-->[robbery].
% Takes Hero mood , abstract theme of ask(present, help,....), and concret subject of ask.  
% Prints generic phrase cause by mood, and expand answer depends on topic.
% Returns AnswerTone.
hero_reacts(HeroMood,Abstract, Subject, AnswerTo, AnswerTone) --> 
									% {writeln(['Get in hero reacts',HeroMood,Abstract, Subject, AnswerTo, AnswerTone])},
									{lex(HeroMood,mood,AnswerTone)},
									replic(answer,AnswerTone),
									[AnswerTo],
									['.'],
									expand_reaction(Abstract,Subject, HeroMood).


% Expands reaction on present event. 
% Here character in some way reacts on what other herobring to him
expand_reaction(present,Present,Mood) --> reaction_on_present(Mood),
										  [Present],
										%   {writeln(['Reaction',Present,Mood])},
										  !.

% Expands restment of other character on present given to him 
expand_reaction(resentment,Present,_) --> ['That was brude from your side! I wasted a lot of time trying to find that'],
										  [Present].

expand_reaction(welcome_speech,Present,_) --> ['I knew you wanted this'],
												[Present],
												['.You are welcome.'].
% His answer depends on his mood  
expand_reaction(help,_,Mood) --> {
								lex(Mood,mood,Intention),
								rand_word([answer,help,Intention], 0, Answer)
							},
							[Answer].

expand_reaction(_,_,_) --> [''].



% Takes hero Mood. Returns phrase, that he can say having such mood.
reaction_on_present(Mood) --> [Phrase],
							{   
								lex(Mood,mood,Tone),
								rand_word([reaction,present,Tone],0,Phrase)
							}.

% Explains what present someone presenting. Returns Present, which is an object. 
ask_explanation(present,Present,_,_) --> [Phrase],
							 {
							   rand_word([present_phrase],0,Phrase)  
							 },
							 np(Present),
							 [.].

ask_explanation(help, SubjectOfAsk, Hero, Location) --> {SubjectOfAsk=help},
										{
										   rand_word([help],0,HelpWithWhat),
										   random_element(
											   ['','I am in panic!',
												'Help me!',
												'I am so frustrated!',
												'I don\'t know what to do'	   
											   ], PanicPhrase)
										},
										[PanicPhrase],
										explain_help(HelpWithWhat,Hero,Location).

explain_help(someone_attacked,Hero,Location) --> antagonist_attacks(Location),
											{
												% writeln(['Look for hero help',Hero,Location]),
												rand_word([how_hero_can_help,Hero],0,HelpBy)
											},
											['.'],
                                            ['He will be stopped only if you', HelpBy, '. Nobody other can\'t do that'].
antagonist_attacks(Location) --> {
									rand_word([antagonist],0,Antagonist),
									rand_word([something_evil],0,Evil)
								},
								[Antagonist,'attacks',Location,'.','He wants to', Evil].

% Returns text of greeting, and what other hero asks our Hero.
hero_speaks(Hero, ToHero, Asks) -->  {
								 	 	random_element(['','Hi',
											 			'Hello',
														'Good afternoon',
														'Good to see you again',
														'Well met',
														'Good day'],GPh)
									},
									 [GPh],
									 [','],
									 [ToHero],
									 ['.'],
									 greeting(Hero),
									 ['.'],
									 replic(replic,Asks),
									  ['.'].



% Return concrete location of hero
% Concrete place is a of preposition and place
hero_location(Location,ConcretePlace) --> [Preposition],
										adj(papp),
										place(Location, Place),
										{
											lex(Place,loc_prep,ProperPrepositions),
											random_element(ProperPrepositions,Preposition),
											ConcretePlace = [Preposition, Place]

										},
										!.

% In case if location,doesn't connected with any prepositions and first predicate will crash
hero_location(Location,ConcretePlace) --> prep(prp),
										  adj(papp),
										  place(Location, ConcretePlace).

hero_action(Mood) --> ['was'],
				v(continous,Mood),
				n.

% Return Mood, Location to introduction
% Firstly tries, to choose mood, asociating it with weather. 
% In case of failure randomly choose between good and bad.
place_descr(Mood, Location) --> weather(Mood),
								{(var(Mood)->random_element([bad,good],Mood);
								 true)},
								[Preposition],
								adj(papp),
								place(Location),
								['.'],
								{	
									lex(Location,loc_prep,ProperPrepositions),
									random_element(ProperPrepositions,Preposition)
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
weather(Type) --> ['It is'],
			      adj(wapp,Type),
			      period.

% Describes what could be the reason of hero mood
mood_reason(HeroMood) --> [Word],{
							lex(HeroMood, mood, Intention),
							rand_word([reas_mood,Intention],0,Word)

							}.



% Returns target noun
np(Target) --> [Article],
		adj(tapp),
		n(Target),
		{choose_article(Target,Article)}.	
		

% article --> [Word],{rand_word([article],0,Word)}.
adj(Target) --> [Article,Word],{rand_word([adj,Target,_],0,Word),choose_article(Word,Article)}.
% Returns random weather and mood associated with it
adj(Target,Mood) --> [Article,Word],{rand_lexem([adj,Target,_],0,lex(Word,_,_,Mood)),choose_article(Word,Article)}.
n --> [Word],{rand_word([n, _],0,Word)}.
% Returns target. Same as word
n(Target) --> [Word],{rand_word([n, _],0,Word),Target=Word}.
prep --> [Word],{rand_word([prep,_],0,Word)}.
prep(Target) --> [Word],{rand_word([prep,Target],0,Word)}.
period --> [Word],{rand_word([period],0,Word)}.

% Location in place is the same is place.
place(Location) --> [Word],{rand_word([place,_],0,Word),Location = Word}.
% Returns concrete location of hero in his current place. It could be room, hall, etc
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





hero(no_adj, ExistingHero, Hero) -->  [Hero],
									{
										choose_diferent([hero],0,ExistingHero,Hero)
									}.

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

