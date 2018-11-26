:- include('probability').

starthand(NUMPLAYERS) :-
    write("Hi! I can help you determine the best move given your hand.\n"),
    write("First, what cards were you dealt? (input like this: jack of diamonds and 7 of spades)\n"),
    readln(L1),
    parseinput(L1, HAND),
    write("Now, what cards are on the table?\n"),
    readln(L2),
    parseinput(L2, COMM),
    chancetowin(HAND, COMM, NUMPLAYERS).

parseinput(WORDS, CARDS) :-
    subtract(WORDS, [of,and], CARDWORDS),
    getcardsfromwords(CARDWORDS, CARDS).

getcardsfromwords([], []).
getcardsfromwords([F], _) :- write("Invalid input.\n"), false.
getcardsfromwords([F,S|T], [card(F, SV)|R]) :- getsuitfromword(S, SV), getcardsfromwords(T, R).

getsuitfromword(KEY, VALUE) :- getvalue(KEY, VALUE, [diamonds, hearts, spades, clubs], [d, h, s, c]).

getvalue(KEY, VALUE, KEYS, VALUES) :- nth0(I, KEYS, KEY), nth0(I, VALUES, VALUE).
