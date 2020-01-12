:-dynamic lex/3.
% Helpful predicates
% Dynamicaly creates connections at the beggining of story, between concretelocations and prepositions
location_preposition([hall, forge, cabinet, bathroom,gym,tent,tavern, pub, prison,cave, 'dwarf hall'], [in, inside, at]).
location_preposition(['fallen tree',spaceship,lodon, edinburg, 'horse back'], [on,under]).



connect_loc_prep([Loc|Locations],Prepositions):-
    length(Locations, L),
    L>0,
    !,
    assert(lex(Loc,loc_prep,Prepositions)),
    connect_loc_prep(Locations,Prepositions).    

connect_loc_prep([Loc],Prepositions):-
    assert(lex(Loc,loc_prep,Prepositions)).

% Times 
lex(continous, times).
lex(present, times).
lex(past, times).



%exlamations
lex('Waahh!',exclam).
lex('Aggghaf!',exclam).
lex('Rrrrrrr!',exclam).


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
lex('John Snow',hero).
lex('Obi-Wan',hero).
lex('Unicorn',hero).
lex('Ragnar',hero).
lex('Snowman',hero).
lex('Winston',hero).
% Articles
lex('the',article).
lex('a',article).

% Replices and their meanings
lex('I need your help ', replic, help).
lex('I have something for you', replic,present).
lex('I bring bad news with me', replic,bad_news).
lex('I here to tell you something good',replic, good_news).
lex('I need you!',replic, help).
lex('It is a good day for you!',replic,present).
lex('It is a good day for you!',replic,good_news).
% Answer
lex('Doesn\'t matter',answer,bad).
lex('Great, thank you!',answer,good).




% wapp - weather adjective could be applied
% papp - place adjectives could be applied 
%Places
lex(castle, place,[hall, forge, cabinet, bathroom]).
lex(spaceship,place,[bridge, gym]).
lex(forest,place,['fallen tree', tent]).
lex(london,place, [cabinet, tavern, pub, prison]).
lex(mountains,place,[cave, 'dwarf hall']).
lex(britan, place,[lodon, edinburg, 'horse back']).

% prp - preposition of place
% prt - preposition of time
% prm - preposition of movement
% Prepositions.
%prt
lex('on',prep,prt).
lex('in',prep,prt).
lex('at',prep,prt).
lex('before',prep,prt).
lex('after',prep,prt).
%prp
lex('at',prep,prp).
lex('on',prep,prp).
lex('in',prep,prp).
lex('inside',prep,prp).
lex('under',prep,prp).
%prm
lex('to',prep,prm).
lex('across',prep,prm).
lex('into',prep,prm).
lex('through',prep,prm).
 %Moods
lex(happy,mood,good).
lex(angry,mood,bad).
lex(cheerful,mood,good).
lex(hopeful,mood,good).
lex(feared,mood,bad).

% Sentence connectors
lex('was',sc,past).
lex('has been', sc, past).
lex('is',sc, present).
lex('is',sc, continous).

% Nouns
lex('guitar',n,s).
lex('magic',n,s).
lex('sword',n,s).
lex('book',n,s).
lex('shield',n,s).
lex('hat',n,s).
lex('backpack',n,s).
lex('goblet',n,s).
lex('clock',n,s).
lex('boot',n,s).
lex('knife',n,s).


% human_appliable - happ
% Participles
lex('risen',participle,nhapp).
lex('fallen',participle,happ).
lex('exited',participle,happ).
lex('interested',participle,happ).

% Greetings 
% Specific greetings for every character

lex('I know something.',greeting, 'John Snow').
lex('Use the Force,', greeting,'Obi-Wan').
lex('I am not a myph.',greeting,'Unicorn').
lex('Odin gave his eye to acquire knowledge...but I would give far more.',greeting,'Ragnar').
lex('Hands down, this is the best day of my life.',greeting,'Snowman').
lex('Success is not final, failure is not fatal: it is the courage to continue that counts. ',greeting,'Winston').



% Adjectives 
% _ - universal adjectives 
% human appliable - happ.
% weather appliable - wapp.
% place appliable - papp
lex('',_,_,_).
lex(rainy,adj,wapp,bad).
lex('giggling',adj,happ,good).
lex('cloudy',adj,wapp,good).
lex('boring',adj,_,good).
lex('funny',adj,_,good).
lex('interesting',adj,_,good).
lex('galactic',adj,papp,good).
lex('space',adj,papp,good).
lex('medival',adj,papp,good).
lex('lonely',adj,happ,good).
lex('angry',adj,happ,bad).
lex('crazy',adj,happ,bad).
lex('cute',adj,happ,good).
lex('good-looking',adj,happ,good).
lex('friendly',adj,happ,good).
lex('graceful',adj,happ,good).
lex('old',adj,happ,good).
lex('unlucky',adj,happ,good).
lex('young',adj,happ,good).

% Format
% lex(word, v, time, list_of_related_prepositions)
% list_of_related_prepositions = [with,at, ...]
% Verbs
lex('play',v,infinitive,[with, on],good).
lex('learn',v,infinitive,[with, at],good).
lex('puffing',v,continous,[at, on],good).
lex('learning',v,continous, [about, to],good).
lex('playing',v,continous, [with, on],good).
lex('giggling',v,continous,[at, about],good).
lex('work',v,infinitive,[with],good).
lex('jump',v,infinitive,[into],good).
lex('run',v,infinitive,[through],good).
lex('sleep',v,infinitive,[under],good).
lex('roaring',v,continous,[at],bad).
lex('killing',v,continous,[''],bad).
lex('robing',v,continous,[the],bad).

