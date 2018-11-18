suit(♦).
suit(♥).
suit(♠).
suit(♣).

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

card(Rank, Suit) :- rank(Rank), suit(Suit).

deck(A) :- findall((Suit, Rank), card(Suit, Rank), A).

value(card(rank(jack), _), 11).
value(card(rank(queen), _), 12).
value(card(rank(king), _), 13).
value(card(rank(ace), _), 14).
value(card(rank(N), _), N) :- number(N).








