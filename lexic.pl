:-dynamic lex/3.
% Times 
lex(continous, times).
lex(present, times).
lex(past, times).

% Periods
lex(day, period).
lex(night, period).
lex(morning, period).

% Heros
lex('John Snow',hero).
lex('Anakin',hero).
lex('Obi-Wan',hero).
lex('Unicorn',hero).

%Places
lex('hall', place).
lex('forest',place).
lex('ship',place).
lex('castle',place).

% Articles
lex('the',article).
lex('a',article).

% papp - place appliable
% Prepositions.
lex('to',prep).
lex('on',prep).
lex('about',prep).
lex('with',prep).
lex('in',prep,papp).
lex('under',prep,papp).
lex('on',prep,papp).

% Sentence connectors
lex('was',sc,past).
lex('is',sc, present).
lex('is',sc, continous).

% Nouns
lex('guitar',n,s).
lex('football',n,s).
lex('magic',n,s).

% _ - universal adjectives 
% human appliable - happ.
% weather appliable - wapp.
% place appliable - papp
% Adjectives 
lex('giggling',adj,happ).
lex('cloudy',adj,wapp).
lex('boring',adj,_).
lex('funny',adj,_).
lex('interesting',adj,_).
lex('galactic',adj,papp).
lex('space',adj,papp).
lex('medival',adj,papp).
lex('lonely',adj,_).



% Verbs
lex('play',v,infinitive).
lex('learn',v,infinitive).
lex('puffing',v,continous).
lex('learning',v,continous).
lex('playing',v,continous).

% human_appliable - happ
% Participles
lex('risen',participle,nhapp).
lex('fallen',participle,happ).
lex('exited',participle,happ).
lex('interested',participle,happ).

% Participle
lex('giggling',participle,happ,continous).



