% Utils to work with grammar:
% choose_different - choose result different from existing
% random_element - choose random element from list
% random_word - choose random word from knowledge base, that satisfy required conditions 
% random_clause - choose random clause from knowledge base
% insert - inserts element in List
% remove_diff_list_from_clause - remove placeholders for differential list, 
% for later usage of clause in differential list
% connect_loc_prep - create records of form lex(Loaction,loc_prep,AllowedPrepositions) 
% for provided locations in knowledge base.   
% process_story - capitilize first leters in words in given story
% choose article - choose article for noun

% Generic method, to randomly choose  Result different from Existing.
choose_diferent(ArgListWithoutGoal, GoalPos, Existing, Result):-
	insert(Goal, GoalPos, ArgListWithoutGoal, LexemArgs),
	RandomLexem =.. [lex|LexemArgs],
	rand_lexem(ArgListWithoutGoal,GoalPos,RandomLexem),
	(\+Goal=Existing->Result=Goal;
	 choose_diferent(ArgListWithoutGoal, GoalPos, Existing, Result)).



random_element(List,Element):-
	length(List,L),
	random(0, L, I),
	nth0(I,List,Element).


insert(Element, 0, List, [Element|List]).
insert(Element, I, [H|T], [H|NT]) :-
    I > 0,
    NI is I - 1,
    insert(Element, NI, T, NT).
 


% Wrapper around rand_lexem, that returns concrete word 
rand_word(IgnoreParamsList, GoalPos, Result):-   	
    rand_lexem(IgnoreParamsList, GoalPos, Lexem),
	Lexem =.. [lex|Params],
    nth0(GoalPos,Params,Result).

% choose random lexem , from lexems that have same 
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
% The reason is - is that aggregate - is an implemantation of bagof, and bagof looks untill he won't fail.
% Because of that in the end we will get a way more bigger number that we wanted 
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
	random_between(1,NumberOfPredicates, I),	
	writeln(I),
	nth_clause(GrammarC, I, R),writeln([GrammarC]),clause(ClauseWithDiffList, _, R),
	writeln('Should enter remove'),
	remove_diff_list_from_clause(ClauseWithDiffList,Result).

% Remove placeholders for Differential list from clause,
% because it needed to later use this clause in grammar clause
remove_diff_list_from_clause(ClauseWithDiff, Result):-
	writeln('Enter remove'),
	ClauseWithDiff =.. [Predicate|Params],
	writeln(['Here',ParamsWithoutDiffList]),	
	length(DiffList, 2),
	append(ParamsWithoutDiffList, DiffList, Params),
	writeln(ParamsWithoutDiffList),	
	Result =.. [Predicate|ParamsWithoutDiffList].

% Doing the same things as remove_diff_list_from_clause/2,
% but instead of empty args in clause, places provided Args.
remove_diff_list_from_clause(ClauseWithDiff, Args,Result):-
	ClauseWithDiff =.. [Predicate|Params],
	length(DiffList, 2),
	append(ParamsWithoutDiffList, DiffList, Params),
	ParamsWithoutDiffList = Args,
	Result =.. [Predicate|Args].


% Connect prepositions with places, where they can be used 
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

% Used in include to check if element is a memeber of given list
is_member_of(List,X):-
    var(X),
    X = is_member_of(List).

is_member_of(List,X):-
    \+member(X,List).

% Capitilizing first letters of word, that goes after period
% br stands for break line
process_story([],[]).
process_story(['.',Word|Story],['.',Capitilized|Other]):-
    \+Word=br,
    fist_to_upper(Word,Capitilized),
	process_story(Story, Other).

process_story([Word|Story],[Word|Result]):-
	process_story(Story, Result).

fist_to_upper(String, Capitilized):-
	string_chars(String, [FirstChar|Last]),
	upcase_atom(FirstChar, Uppercased),
	string_chars(Capitilized,[Uppercased|Last]).

% writes a story, placing newlines in places of quotes
tell_story(Story):-    
    foreach(member(X,Story),
          (X=br) -> nl;
          (write(X),write(' '))).        
    
% choose article for noun
choose_article(Word,'an'):-
            string_chars(Word, [First|_]),
            vowel(First).
choose_article(_,'a').
            


