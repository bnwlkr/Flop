% to set the current games pocket: asserta(pocket(card_,_), card(_,_)).
% to set the table: retract + assert(table([card(_,_),...]))
% @< used as arbitrary ordering to remove duplicate answers

rank(    2).
rank(    3).
rank(    4).
rank(    5).
rank(    6).
rank(    7).
rank(    8).
rank(    9).
rank(   10).
rank( jack).
rank(queen).
rank( king).
rank(  ace).

suit(d).
suit(h).
suit(s).
suit(c).

card(Rank, Suit) :- rank(Rank), suit(Suit).

% card values
value(card(jack, _), 11).
value(card(queen, _), 12).
value(card(king, _), 13).
value(card(ace, _), 14).
value(card(N, _), N) :- rank(N), number(N).


% for straights
order(card(ace, _), 1).
order(card(R,S), V):- dif(R, ace), value(card(R,S), V).

deck(A) :- findall((Suit, Rank), card(Suit, Rank), A).

indexofhelp(E, [E | _], I, I).
indexofhelp(E, [H | T], I, N) :-
    dif(E, H),
    NI is I+1,
    indexofhelp(E, T, NI, N).
indexof(E, LIST, N) :-
    indexofhelp(E, LIST, 0, N).

maxvalue([H], H).
maxvalue([H | T], H) :-
    maxvalue(T, M),
    value(H, HV),
    value(M, MV),
    HV > MV.
maxvalue([H | T], M)
    maxvalue(T, M),
    value(H, HV),
    value(M, MV),
    HV < MV.

high([card(FV, FS), card(SV, SS)], [card(FV, FS)]) :-
    value(card(FV, FS), VF),
    value(card(SV, SS), VS),
    VF > VS.
high([card(FV, FS), card(SV, SS)], [card(SV, SS)]) :-
    value(card(FV, FS), VF),
    value(card(SV, SS), VS),
    VS > VF.

pair(CARDS, [card(V1, S1), card(V2, S2)]) :-
    length(CARDS, LENGTH),
    LENGTH < 8,
    member(card(V1, S1), CARDS),
    member(card(V2, S2), CARDS),
    dif(card(V1, S1), card(V2, S2)),
    indexof(card(V1, S1), CARDS, I1),
    indexof(card(V2, S2), CARDS, I2),
    I1 < I2,
    V1 = V2.

twopair(CARDS, [card(V1, S1), card(V2, S2), card(V3, S3), card(V4, S4)]) :-
    length(CARDS, LENGTH),
    LENGTH < 8,
    member(card(V1, S1), CARDS),
    member(card(V2, S2), CARDS),
    member(card(V3, S3), CARDS),
    member(card(V4, S4), CARDS),
    dif(card(V1, S1), card(V2, S2)),
    dif(card(V1, S1), card(V3, S3)),
    dif(card(V1, S1), card(V4, S4)),
    dif(card(V2, S2), card(V3, S3)),
    dif(card(V2, S2), card(V4, S4)),
    dif(card(V3, S3), card(V4, S4)),
    dif(V1, V3),
    indexof(card(V1, S1), CARDS, I1),
    indexof(card(V2, S2), CARDS, I2),
    indexof(card(V3, S3), CARDS, I3),
    indexof(card(V4, S4), CARDS, I4),
    I1 < I2,
    I1 < I3,
    I3 < I4,
    V1 = V2,
    V3 = V4.

three(CARDS, [card(V1, S1), card(V2, S2), card(V3, S3)]) :-
    length(CARDS, LENGTH),
    LENGTH < 8,
    member(card(V1, S1), CARDS),
    member(card(V2, S2), CARDS),
    member(card(V3, S3), CARDS),
    dif(card(V1, S1), card(V2, S2)),
    dif(card(V1, S1), card(V3, S3)),
    dif(card(V2, S2), card(V3, S3)),
    indexof(card(V1, S1), CARDS, I1),
    indexof(card(V2, S2), CARDS, I2),
    indexof(card(V3, S3), CARDS, I3),
    I1 < I2,
    I2 < I3,
    V1 = V2,
    V1 = V3.

straighthelp(_, _, 0, []).
straighthelp(CARDS, card(E, S1), NUM, BEST) :-
    NUM > 0,
    member(card(N, S2), CARDS),
    value(card(E, S1), EV),
    NV is EV-1,
    value(card(N, S2), NV),
    SUB is NUM-1,
    straighthelp(CARDS, card(N, S2), SUB, SUBBEST),
    append([card(N, S2)], SUBBEST, BEST).

straight(CARDS, BEST) :-
    length(CARDS, LENGTH),
    LENGTH < 8,
    straightflushhelp(CARDS, E, 4, SUBBEST),
    append([E], SUBBEST, BEST),
    member(E, CARDS).

fullhouse(CARDS, BEST) :-
    length(CARDS, LENGTH),
    LENGTH < 8,
    pair(CARDS, [card(PV, PS) | PT]),
    three(CARDS, [card(TV, TS) | TT]),
    dif(PV, TV),
    append([card(PV, PS) | PT], [card(TV, TS) | TT], BEST).

four(CARDS, [card(V1, S1), card(V2, S2), card(V3, S3), card(V4, S4)]) :-
    length(CARDS, LENGTH),
    LENGTH < 8,
    member(card(V1, S1), CARDS),
    member(card(V2, S2), CARDS),
    member(card(V3, S3), CARDS),
    member(card(V4, S4), CARDS),
    dif(card(V1, S1), card(V2, S2)),
    dif(card(V1, S1), card(V3, S3)),
    dif(card(V1, S1), card(V4, S4)),
    dif(card(V2, S2), card(V3, S3)),
    dif(card(V2, S2), card(V4, S4)),
    dif(card(V3, S3), card(V4, S4)),
    indexof(card(V1, S1), CARDS, I1),
    indexof(card(V2, S2), CARDS, I2),
    indexof(card(V3, S3), CARDS, I3),
    indexof(card(V4, S4), CARDS, I4),
    I1 < I2,
    I2 < I3,
    I3 < I4,
    V1 = V2,
    V1 = V3,
    V1 = V4.

straightflushhelp(_, _, 0, []).
straightflushhelp(CARDS, card(E, S), NUM, BEST) :-
    NUM > 0,
    member(card(N, S), CARDS),
    value(card(E, S), EV),
    NV is EV-1,
    value(card(N, S), NV),
    SUB is NUM-1,
    straightflushhelp(CARDS, card(N, S), SUB, SUBBEST),
    append([card(N, S)], SUBBEST, BEST).

straightflush(CARDS, BEST) :-
    length(CARDS, LENGTH),
    LENGTH < 8,
    LENGTH > 4,
    straightflushhelp(CARDS, E, 4, SUBBEST),
    append([E], SUBBEST, BEST),
    member(E, CARDS),
    maxvalue(E, CARDS).

royalflush(CARDS, [card(ace, S), card(king, S), card(queen, S), card(jack, S), card(10, S)]) :-
    length(CARDS, LENGTH),
    LENGTH < 8,
    member(card(ace, S), CARDS),
    member(card(king, S), CARDS),
    member(card(queen, S), CARDS),
    member(card(jack, S), CARDS),
    member(card(10, S), CARDS).

besthand(HAND, COMM, BEST) :-
    append(HAND, COMM, CARDS),
    royalflush(CARDS, BEST).
besthand(HAND, COMM, BEST) :-
    append(HAND, COMM, CARDS),
    \+ royalflush(CARDS, _),
    straightflush(CARDS, BEST).
besthand(HAND, COMM, BEST) :-
    append(HAND, COMM, CARDS),
    \+ royalflush(CARDS, _),
    \+ straightflush(CARDS, _),
    four(CARDS, BEST).
besthand(HAND, COMM, BEST) :-
    append(HAND, COMM, CARDS),
    \+ royalflush(CARDS, _),
    \+ straightflush(CARDS, _),
    \+ four(CARDS, _),
    fullhouse(CARDS, BEST).
besthand(HAND, COMM, BEST) :-
    append(HAND, COMM, CARDS),
    \+ royalflush(CARDS, _),
    \+ straightflush(CARDS, _),
    \+ four(CARDS, _),
    \+ fullhouse(CARDS, _),
    straight(CARDS, BEST).
besthand(HAND, COMM, BEST) :-
    append(HAND, COMM, CARDS),
    \+ royalflush(CARDS, _),
    \+ straightflush(CARDS, _),
    \+ four(CARDS, _),
    \+ fullhouse(CARDS, _),
    \+ straight(CARDS, _),
    three(CARDS, BEST).
besthand(HAND, COMM, BEST) :-
    append(HAND, COMM, CARDS),
    \+ royalflush(CARDS, _),
    \+ straightflush(CARDS, _),
    \+ four(CARDS, _),
    \+ fullhouse(CARDS, _),
    \+ straight(CARDS, _),
    \+ three(CARDS, _),
    twopair(CARDS, BEST).
besthand(HAND, COMM, BEST) :-
    append(HAND, COMM, CARDS),
    \+ royalflush(CARDS, _),
    \+ straightflush(CARDS, _),
    \+ four(CARDS, _),
    \+ fullhouse(CARDS, _),
    \+ straight(CARDS, _),
    \+ three(CARDS, _),
    \+ twopair(CARDS, _),
    pair(CARDS, BEST).
besthand(HAND, COMM, BEST) :-
    append(HAND, COMM, CARDS),
    \+ royalflush(CARDS, _),
    \+ straightflush(CARDS, _),
    \+ four(CARDS, _),
    \+ fullhouse(CARDS, _),
    \+ straight(CARDS, _),
    \+ three(CARDS, _),
    \+ twopair(CARDS, _),
    \+ pair(CARDS, _),
    high(HAND, BEST).
