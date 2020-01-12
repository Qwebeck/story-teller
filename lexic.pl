% Knowledge base. lex/n - describing fact.
% location_preposition(ListofLocations,AllowedPrepositions) - list of prepositions,
% that could be used with locations in list of locations  
:- consult(utils).
:- (dynamic lex/3).
% Helpful predicates
% Dynamicaly creates connections at the beggining of story, between concretelocations and prepositions
location_preposition([hall, forge, cabinet, bathroom, gym, tent, tavern, pub, prison, cave, 'dwarf hall', castle, forest, 'London', chamber], [in, inside, at]).

location_preposition(['Britan', 'Edinburg'], [in]).

location_preposition([mountains, forest], [under]).

location_preposition(['fallen tree', spaceship, 'horse back', 'cargo bay'], [on, under]).

vowel(a).
vowel(e).
vowel(u).
vowel(i).
vowel(o).



                
% Times 
lex(continous, times).
lex(present, times).
lex(past, times).

%exlamations
lex('Waahh!', exclam).
lex('Aggghaf!', exclam).
lex('Rrrrrrr!', exclam).


% Periods
lex(day, period).
lex(night, period).
lex(morning, period).
lex(midnight, period).
lex(afternoon, period).
lex(teatime, period).
lex(siesta, period).
lex(halloween, period).

% Heros
lex('John Snow', hero).
lex('Obi-Wan', hero).
lex('Unicorn', hero).
lex('Ragnar', hero).
lex('Winston', hero).
%Antagonists
lex('Tom Riddle',antagonist).
lex('Foltest',antagonist).
lex('Grinch',antagonist).
% Evil actions of antagonist
lex(' destroy Christmas',something_evil).
lex(' concure us',something_evil).
lex(' ruin the King',something_evil).
lex('rule the Earth',something_evil).



% Articles
lex(the, article).
lex(a, article).

% Variations for help
lex(someone_attacked, help).
% Unimplemented brancehs
%lex(have_no_money, help).
%lex(have_bad_mood, help).

% Present phrases
lex('I found ', present_phrase).
lex('I bought ', present_phrase).
lex('I bring ', present_phrase).
lex('I decided to buy you', present_phrase).

% Reasons of mood
lex('without reason', reas_mood, _).
lex('because not so far ago he had a good meal', reas_mood, good).
lex('because his leg was broken', reas_mood, bad).
lex('because he is living', reas_mood, good).
lex('because the life is great', reas_mood, good).
lex('because people are angry', reas_mood, bad).
lex('because he is hungry', reas_mood, bad).

% Possible reactions depending on tongue 
lex(resentment, reaction, bad).
lex(welcome_speech, reaction, good).





% Replices and their meanings
lex('I need your help, ', replic, help).
lex('I have something for you', replic, present).
%lex('I bring bad news with me', replic,bad_news).
%lex('I here to tell you something good',replic, good_news).
lex('I need you!',replic, help).
lex('It is a good day for you!', replic, present).
%lex('It is a good day for you!',replic,good_news).

% Present phrases


% Answer
lex('Doesn\'t matter', answer, bad).
lex('I don\'t care', answer, bad).
lex('I hadn\'t expected to hear that from you', answer, bad).
lex('Great, thank you!', answer, good).
lex('Glad to hear it.', answer, good).





% wapp - weather adjective could be applied
% papp - place adjectives could be applied 
%Places
lex(castle, place, [hall, forge, cabinet, bathroom]).
lex(spaceship, place, ['cargo bay', chamber]).
lex(forest, place, ['fallen tree', tent]).
lex('London', place, [cabinet, tavern, pub, prison]).
lex(mountains, place, [cave, 'dwarf hall']).
lex('Britan', place, ['London', 'Edinburg', 'horse back']).

% prp - preposition of place
% prt - preposition of time
% prm - preposition of movement
% Prepositions.
%prt
lex(on, prep, prt).
lex(in, prep, prt).
lex(at, prep, prt).
lex(before, prep, prt).
lex(after, prep, prt).
%prp
lex(at, prep, prp).
lex(on, prep, prp).
lex(in, prep, prp).
lex(inside, prep, prp).
lex(under, prep, prp).
%prm
lex(to, prep, prm).
lex(across, prep, prm).
lex(into, prep, prm).
lex(through, prep, prm).

 %Moods
lex(happy, mood, good).
lex(angry, mood, bad).
lex(cheerful, mood, good).
lex(hopeful, mood, good).
lex(feared, mood, bad).

% Sentence connectors
lex(was, sc, past).
lex('has been', sc, past).
lex(is, sc, present).
lex(is, sc, continous).

% Nouns
lex(guitar, n, s).
lex(magic, n, s).
lex(sword, n, s).
lex(book, n, s).
lex(shield, n, s).
lex(hat, n, s).
lex(backpack, n, s).
lex(goblet, n, s).
lex(clock, n, s).
lex(boot, n, s).
lex(knife, n, s).


% human_appliable - happ
% Participles
lex(risen, participle, nhapp).
lex(fallen, participle, happ).
lex(exited, participle, happ).
lex(interested, participle, happ).

% Greetings 
% Сatchphrase for every character
lex('I know something', greeting, 'John Snow').
lex('Use the Force', greeting, 'Obi-Wan').
lex('I am not a myph', greeting, 'Unicorn').
lex('Odin gave his eye to acquire knowledge...but I would give far more', greeting, 'Ragnar').
% lex('Hands down, this is the best day of my life', greeting, 'Snowman').
lex('Success is not final, failure is not fatal: it is the courage to continue that counts ', greeting, 'Winston').
% How every hero can help
lex('will give a Ghost', how_hero_can_help, 'John Snow').
lex('will rise the North', how_hero_can_help, 'John Snow').
lex('will rise the Night Watch', how_hero_can_help, 'John Snow').
lex('will teach me how to use Force',how_hero_can_help,'Obi-Wan').
lex('will find Yoda',how_hero_can_help,'Obi-Wan').
lex('will find magic sword',how_hero_can_help,'Unicorn').
lex('will rise Vikings',how_hero_can_help,'Ragnar').
lex('will find a Great Drakkar', how_hero_can_help,'Ragnar').
lex('will connect us',how_hero_can_help,'Winston').


% Answers on something
lex('Sorry,I can\'t help you', answer, help,bad).
lex('I will help you',answer, help,good).

% Syntax
% lex(Phrase,reaction,Subject,Intention)
lex('That is a great ', reaction,present, good).
lex('I was dreaming about ', reaction,present, good).
lex('I hate ', reaction,present, bad).
lex('I very appreciate this', reaction,present, good).
lex('Don\'t show me this', reaction,present, bad).
lex('', reaction,help, bad).
lex('', reaction,help, good).




% Adjectives 
% _ - universal adjectives 
% human appliable - happ.
% weather appliable - wapp.
% place appliable - papp
% thing appliable -tapp
% Syntax
% lex(Word,adj,Target,Intention)
lex('', adj, _, _).
lex(rainy, adj, wapp, bad).
lex(giggling, adj, happ, good).
lex(cloudy, adj, wapp, good).
lex(boring, adj, papp, good).
lex(boring, adj, tapp, good).
lex(funny, adj, tapp, good).
lex(interesting, adj, papp, good).
lex(interesting, adj, tapp, good).
lex(galactic, adj, papp, good).
lex(space, adj, papp, good).
lex(medival, adj, papp, good).
lex(lonely, adj, papp, good).
lex(angry, adj, happ, bad).
lex(crazy, adj, happ, bad).
lex(cute, adj, papp, good).
lex('good-looking', adj, happ, good).
lex(friendly, adj, happ, good).
lex(graceful, adj, happ, good).
lex(old, adj, happ, good).
lex(unlucky, adj, happ, good).
lex(young, adj, happ, good).
lex(long, adj, tapp, _).
lex(nice, adj, tapp, _).
lex(ancient, adj, _, _).
lex(cool, adj, tapp, _).
lex(charmning, adj, _, good).
lex(crowded, adj, papp, bad).
lex(famous, adj, _, good).
lex(magical, adj, _, good).
lex(quiet, adj, _, good).
lex(tired, adj, happ, good).
lex(tired, adj, happ, bad).

% Format
% lex(word, v, time, list_of_related_prepositions)
% list_of_related_prepositions = [with,at, ...]
% Verbs
lex(play, v, infinitive, [with, on], good).
lex(learn, v, infinitive, [with, at], good).
lex(puffing, v, continous, [at, on], good).
lex(learning, v, continous, [about, to], good).
lex(playing, v, continous, [with, on], good).
lex(giggling, v, continous, [at, about], good).
lex(work, v, infinitive, [with], good).
lex(jump, v, infinitive, [into], good).
lex(run, v, infinitive, [through], good).
lex(sleep, v, infinitive, [under], good).
lex(roaring, v, continous, [at], bad).
lex(killing, v, continous, [''], bad).
lex(robing, v, continous, [the], bad).
lex(bying, v, continous, [''], _).
lex(hiding, v, continous, [''], _).
lex(selling, v, continous, [''], _).
lex(shooting, v, continous, [''], bad).
lex(painting, v, continous, [''], good).

