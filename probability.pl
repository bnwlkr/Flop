:- include('poker.pl').

% given a list of cards in your hand and a set of community cards, find all hands that beat yours
whatbeatsme(MYHAND, COMM, HANDS, R) :- append(MYHAND, COMM, MYCARDS), findall(HAND, (member(HAND, HANDS), append(HAND, COMM, THEIRCARDS), winner(MYCARDS, THEIRCARDS, p2)), R).

% get all cards
allcards(CARDS) :- findall(card(V, S), (rank(V), suit(S)), CARDS).

% given a list of cards, produce all other cards
nottaken(TAKEN, NOTTAKEN) :- allcards(ALL), subtract(ALL, TAKEN, NOTTAKEN).

% get all possible untaken hands
possiblehands(NOTTAKEN, R) :- findall([card(V1, S1), card(V2, S2)], (member(card(V1, S1), NOTTAKEN), member(card(V2, S2), NOTTAKEN), greater(card(V1, S1), card(V2, S2))), R).

chancetowin(MYHAND, COMM, NUMPLAYERS) :- append(MYHAND, COMM, MYCARDS), nottaken(MYCARDS, NOTTAKEN), possiblehands(NOTTAKEN, HANDS), whatbeatsme(MYHAND, COMM, HANDS, BEATSME), length(HANDS, T), length(BEATSME, B), write("There are "), write(T), write(" total possible hands and "), write(B), write(" that beat yours.\n").
