:- include('poker.pl').
:- use_module(library(clpfd)).

% given a list of cards in your hand and a set of community cards, find all hands that beat yours
whatbeatsme(MYHAND, COMM, HANDS, R) :- append(MYHAND, COMM, MYCARDS), findall(HAND, (member(HAND, HANDS), append(HAND, COMM, THEIRCARDS), winner(MYCARDS, THEIRCARDS, p2)), R).

% get all cards
allcards(CARDS) :- findall(card(V, S), (rank(V), suit(S)), CARDS).

% given a list of cards, produce all other cards
nottaken(TAKEN, NOTTAKEN) :- allcards(ALL), subtract(ALL, TAKEN, NOTTAKEN).

% get all possible untaken hands
possiblehands(NOTTAKEN, R) :- findall([card(V1, S1), card(V2, S2)], (member(card(V1, S1), NOTTAKEN), member(card(V2, S2), NOTTAKEN), greater(card(V1, S1), card(V2, S2))), R).

chancetowin(MYHAND, COMM) :-
    percentyoubeat(MYHAND, COMM),
    bluffopp(MYHAND, COMM).

percentyoubeat(MYHAND, COMM) :-
    append(MYHAND, COMM, TAKEN), nottaken(TAKEN, NOTTAKEN),
    possiblehands(NOTTAKEN, HANDS), length(HANDS, T),
    whatbeatsme(MYHAND, COMM, HANDS, BEATSME), length(BEATSME, L),
    P is round(100 * L / T), write(P), write("% of possible hands beat yours.\n").

bluffopp(MYHAND, COMM) :-
    append(MYHAND, COMM, MYCARDS), besthand(MYCARDS, BH), ranking(BH, [H1|_]),
    ((H1 < 200, nottaken(COMM, NOTTAKEN), possiblehands(NOTTAKEN, HANDS),
    member(H, HANDS), append(COMM, H, CARDS), besthand(CARDS, TBH), ranking(TBH, [H2|_]),
    H2 > 500, !, write("There is potential for good hands here and yours has low value, this could be a good time to bluff.\n"));
    write("This is not a good time to bluff.\n")).


topN(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
topN(_, [], []).
topN(N, CARDS, [X|Ys]) :- M is N-1, maxrank(CARDS, X), select(X, CARDS, REST), topN(M, REST, Ys).
