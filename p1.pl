/*
** TianHao Ye, ID: 1014747
** Project 2, COMP30020 Declarative programming
** 2021 Semester 2

** Purpose: A fill-in puzzle solver written in Prolog
** 
** Rule: A word is selected from a provided wordlist to be filled in an 
** empty slot of the puzzle at one time, this process continues until all the words 
** in wordlist are filled in the puzzle, and every word can be used exactly once.
** The basic strategy adopted in the solver goes that: 
** 1. Collect all the fillable slots in the puzzle 2. for every slot, collect its 
** matched words 3. fill words into slots, this is achieved by finding the slot 
** with least matched words, and randomly choose a matched word and fill that in, 
** then remove the filled-in word and ununifiable words from other slots' matched 
** word list, the program will backtrack over permutations of matched words until 
** the puzzle is completed. The special thing about prolog is that it initially 
** replaces every square (_) in puzzle with a logical free variable, it means that 
** once we unify one slot with a word, the squares intersect with other slots will 
** be automatically binded with the alphabet.
**
**
**
**
**
*/

:- ensure_loaded(library(clpfd)).

puzzle_solution(Puzzle, WordList) :-
    collect_slots(Puzzle, Slots),
    collect_pairs(Slots, WordList, SlotWordsPairs),
    match(Puzzle, SlotWordsPairs).

%----------------------------------------------------------------------%
% collect_slots(+Puzzle, -Slots)
% collect horizontally and vertically slots in the puzzle 
collect_slots(Puzzle, Slots) :-
    collect_slots_from_one_direction(Puzzle, HSlots),
    transpose(Puzzle, VPuzzle),
    collect_slots_from_one_direction(VPuzzle, VSlots),
    % remove unfillable slots
    collect_legal_slots(HSlots, LegalHSlots),
    collect_legal_slots(VSlots, LegalVSlots),
    append(LegalHSlots, LegalVSlots, Slots).

% collect_legal_slots(+Slots, -LegalSlots)
% remove redundant slot e.g. one square slot
collect_legal_slots(Slots, LegalSlots) :-
    cls(Slots, [], LegalSlots).
cls([], Acc, Acc).
cls([S|Rs], Acc, LegalSlots) :-
    length(S, Len),
    (Len >1 ->
    append(Acc, [S], AccNew)
    ;
    AccNew = Acc
    ),
    cls(Rs, AccNew, LegalSlots).
    
% collect_slots_from_one_direction(+Puzzle, -SlotsD)  
% collect slots in the puzzle from one direction
collect_slots_from_one_direction(Puzzle, SlotsD):-
    csfod(Puzzle, [], SlotsD).
csfod([], Acc, Acc).
csfod([R|Rs], Acc, SlotsD) :-
    collect_slots_from_one_row(R, SlotsR),
    append(Acc, SlotsR, AccNew),
    csfod(Rs, AccNew, SlotsD).

% collect_slots_from_one_row(+R, -SlotsR)
% collect slots in the puzzle from one row
collect_slots_from_one_row(R, SlotsR) :-
    csfor(R, [], [], SlotsR).
% base case 1: exploring row empty, accumulated slot empty
csfor([], [], AccSlots, AccSlots).
% base case 2: exploring row empty, accumulated slot empty, add accumulated slot 
% to output slot list
csfor([], AccCurSlot, AccSlots, SlotsR) :-
    AccCurSlot \= [],
    append(AccSlots, [AccCurSlot], AccSlotsNew),
    csfor([], [], AccSlotsNew, SlotsR).
% normal recursive case    
csfor([E|Es], AccCurSlot, AccSlots, SlotsR):-
    % meet #
    (\+ \+ (E == '#') ->
        % if accumulated slot is non-empty, add it to output slot list
        (AccCurSlot \= [] ->
        append(AccSlots, [AccCurSlot], AccSlotsNew)
        ;
        AccSlotsNew = AccSlots
        ),
        % otherwise reset current list to empty
        AccCurSlotNew = []
    ;
    % meet unbound variable, add it to current slot
    append(AccCurSlot, [E], AccCurSlotNew),
    AccSlotsNew = AccSlots          
    ),
    csfor(Es, AccCurSlotNew, AccSlotsNew, SlotsR).

%----------------------------------------------------------------------%
% collect_pairs(+L1, +L2, -Pairs)
% collect slot-words matching pairs, where words are a list of unifiable word to
% the slot
collect_pairs(L1, L2, Pairs) :-
    cp(L1, L2, [], Pairs).
cp([], _, Acc, Acc).
cp([E1|Es], L2, Acc, Pairs) :-
    setof(E2, (member(E2, L2), \+ \+ (E1 = E2)), E1E2Pairs),
    append(Acc, [E1-E1E2Pairs], NewAcc),
    cp(Es, L2, NewAcc, Pairs).

%----------------------------------------------------------------------%
% match(+Puzzle, +SWPairs)
% continuously fill in slots with words (backtrack if fail) until completed
match(_, []).
match(Puzzle, SWPairs) :-
    % select the slot of fewest mathching words
    select_minimal_matching(SWPairs, CurSlot-Words, RestSWPairs),
    % permutation over words to unnify with the slot
    member(SelectW, Words),
    CurSlot = SelectW,
    % for the rest of Slot-words pairs, remove the unified word and other ununifiable words.
    reduce_pairs(SelectW, RestSWPairs, FilteredRestSWPairs),
    match(Puzzle, FilteredRestSWPairs).

% select_minimal_matching(+Pairs, -MinSWPair, -RestPairs)
% sorting slot-words pairs to find the fewest mathcing slot
select_minimal_matching(Pairs, MinSWPair, RestPairs) :-
    quicksort(Pairs, SortedPairs),
    nth0(0, SortedPairs, MinSWPair, RestPairs).

% quicksort(+[Head|Tail], -Sorted)
% a sorting algorithm with time complexity nlog(n)
quicksort([], []).
quicksort([Head|Tail], Sorted) :- 
    pivot(Head, Tail, Left, Right), 
    quicksort(Left, SortedLeft), 
    quicksort(Right, SortedRight), 
    append(SortedLeft, [Head|SortedRight], Sorted).

% sub helper function of quicksort
pivot(_, [], [], []).
pivot(Y-YWords, [X-XWords|Tail], [X-XWords|LessOrEqualThan], GreaterThan) :-
    length(XWords, LenX), length(YWords, LenY),
    LenX =< LenY, 
    pivot(Y-YWords, Tail, LessOrEqualThan, GreaterThan). 
pivot(Y-YWords, [X-XWords|Tail], LessOrEqualThan, [X-XWords|GreaterThan]) :- 
    length(XWords, LenX), length(YWords, LenY),
    LenX > LenY, 
    pivot(Y-YWords, Tail, LessOrEqualThan, GreaterThan).

% reduce_pairs(+Word, +Pairs, +PairsNew)
% remove the unified word and other ununifiable words in other slot-words 
% mathcing pairs
reduce_pairs(Word, Pairs, PairsNew) :-
    rp(Word, Pairs, [], PairsNew).
rp(_, [], Acc, Acc).
rp(W, [S-Words|Rs], Acc, PairsNew) :-
    (member(W, Words) ->
    select(W, Words, WordsNew);
    WordsNew = Words
    ),
    remove_redundant_words(S, WordsNew, FilteredWordsNew),
    append(Acc, [S-FilteredWordsNew], AccNew),
    rp(W, Rs, AccNew, PairsNew).

remove_redundant_words(S, Words, FilteredWords):-
    rrw(S, Words, [], FilteredWords).
rrw(_, [], Acc, Acc).
rrw(S, [W|Ws], Acc, FilteredWords) :-
    % if the word still unifiable with the slot, add it to matching pairs
    (\+ \+ (S = W) ->
    append(Acc, [W], AccNew);
    AccNew = Acc
    ),
    rrw(S, Ws, AccNew, FilteredWords).
    
