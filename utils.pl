random_element(List,Element):-
	length(List,L),
	random(0, L, I),
	nth0(I,List,Element).


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

connect_loc_prep([],_).

connect_loc_prep([Loc|Locations],Prepositions):-
    (\+lex(Loc,loc_prep,_) ->
    assertz(lex(Loc,loc_prep,Prepositions));
    add_prepositions(Loc,Prepositions)
    ),
    connect_loc_prep(Locations,Prepositions).   


% Add preposition to list of available prepositions for place
add_prepositions(Loc,Prepositions):-
    lex(Loc,loc_prep,ExPrep),
    retractall(lex(Loc,loc_prep,_)),
    include(is_member_of(ExPrep), Prepositions, FilteredPrep),
    append(ExPrep,FilteredPrep,New),
    assertz(lex(Loc,loc_prep,New)).

is_member_of(List,X):-
    var(X),
    X = is_member_of(List).

is_member_of(List,X):-
    \+member(X,List).

% Capitilizing first letters of word, that goes after period
process_story([],[]).
process_story(['.',Word|Story],['.',Capitilized|Other]):-
	fist_to_upper(Word,Capitilized),
	process_story(Story, Other).
process_story([Word|Story],[Word|Result]):-
	process_story(Story, Result).

fist_to_upper(String, Capitilized):-
	string_chars(String, [FirstChar|Last]),
	upcase_atom(FirstChar, Uppercased),
	string_chars(Capitilized,[Uppercased|Last]).
	

