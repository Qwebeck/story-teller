:-dynamic lex/3.
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
lex('Anakin',hero).
lex('Obi-Wan',hero).
lex('Unicorn',hero).
lex('Ragnar',hero).
lex('Snowman',hero).


% wapp - weather adjective could be applied
% papp - place adjectives could be applied 
%Places
lex('hall', place).
lex('ship',place).
lex('castle',place).
lex('forest',place).
lex('mountains',place).
lex('Britan', place).
lex('Island', place).

% Articles
lex('the',article).
lex('a',article).


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
 
%lex('to',prep).
%lex('on',prep).
%lex('about',prep).
%lex('with',prep).
%lex('in',prep,papp).
%lex('under',prep,papp).
%lex('on',prep,papp).


% Sentence connectors
lex('was',sc,past).
lex('has been', sc, past).
lex('is',sc, present).
lex('is',sc, continous).

% Nouns
lex('guitar',n,s).
lex('football',n,s).
lex('magic',n,s).
lex('sword',n,s).
lex('north',n,s).
lex('book',n,s).
lex('shield',n,s).
lex('hat',n,s).
lex('backpack',n,s).
lex('goblet',n,s).
lex('clock',n,s).
lex('dragon',n,s).


% human_appliable - happ
% Participles
lex('risen',participle,nhapp).
lex('fallen',participle,happ).
lex('exited',participle,happ).
lex('interested',participle,happ).

% Participle


% Adjectives 
% _ - universal adjectives 
% human appliable - happ.
% weather appliable - wapp.
% place appliable - papp
lex('giggling',adj,happ,good).
lex('cloudy',adj,wapp,_).
lex('boring',adj,_,_).
lex('funny',adj,_,_).
lex('interesting',adj,_,_).
lex('galactic',adj,papp,_).
lex('space',adj,papp,_).
lex('medival',adj,papp,_).
lex('lonely',adj,happ,_).
lex('angry',adj,happ,bad).
lex('crazy',adj,happ,bad).
lex('cute',adj,happ,good).
lex('good-looking',adj,happ,_).
lex('friendly',adj,happ,good).
lex('graceful',adj,happ,good).
lex('old',adj,happ,_).
lex('unlucky',adj,happ,_).
lex('young',adj,happ,_).

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

