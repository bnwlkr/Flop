suit(♦).
suit(♥).
suit(♠).
suit(♣).

rank(  ace).
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

card(Rank, Suit) :- rank(Rank), suit(Suit).


deck(A) :- findall((Suit, Rank), card(Suit, Rank), A).









