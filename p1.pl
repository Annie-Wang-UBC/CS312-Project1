% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional prepositional phrase.
noun_phrase(T0,T4,Obj,C0,C4) :-
    det(T0,T1,Obj,C0,C1),
    adjectives(T1,T2,Obj,C1,C2),
    noun(T2,T3,Obj,C2,C3),
    mp(T3,T4,Obj,C3,C4).

% Determiners are ignored in this oversimplified example.
det([the | T],T,_,C,C).
det([a | T],T,_,C,C).
det(T,T,_,C,C).

% adjectives consist of a sequence of adjectives.
adjectives(T,T,_,C,C).
adjectives(T0,T2,Obj,C0,C2) :-
    adjective(T0,T1,Obj,C0,C1),
    adjectives(T1,T2,Obj,C1,C2).

% An optional modifying phrase / relative clause is either
% nothing or
% a prepositional phrase or
% that followed by a verb phrase
mp(T,T,_,C,C).
mp(T0,T2,O1,C0,C2) :-
    reln(T0,T1,O1,O2,C0,C1),
    noun_phrase(T1,T2,O2,C1,C2).
mp([that|T0],T2,O1,C0,C2) :-
    reln(T0,T1,O1,O2,C0,C1),
    noun_phrase(T1,T2,O2,C1,C2).
mp([is|T0],T2,O1,C0,C2) :-
    reln(T0,T1,O1,O2,C0,C1),
    noun_phrase(T1,T2,O2,C1,C2).

% DICTIONARY

adjective([bought,by,Bidder | T],T,Obj,C,[bidder(Bidder,Obj)|C]).

noun([item | T],T,Obj,C,[item(Obj)|C]).
noun([bidder | T],T,Obj,C,[bidder(Obj)|C]).
noun([X | T],T,X,C,C) :- item(X).
noun([X | T],T,X,C,C) :- bidder(X).

reln([the,starting,price,of | T],T,O1,O2,C,[starting_price(O2,O1)|C]).
reln([the,bidding,price,of | T],T,O1,O2,C,[bidding_price(O2,O1)|C]).
reln([the,profit,of | T],T,O1,O2,C,[profit(O2,O1)|C]).
reln([bought,by | T],T,O1,O2,C,[bidder(O2,O1)|C]).
reln([bought | T],T,O1,O2,C,[bidder(O1,O2)|C]).

% question(Question,QR,Object,Q0,Query) is true if Query provides an answer about Object to Question
question([is | T0],T2,Obj,C0,C2) :-
    noun_phrase(T0,T1,Obj,C0,C1),
    mp(T1,T2,Obj,C1,C2).
question([what,is | T0],T1,Obj,C0,C1) :-
    mp(T0,T1,Obj,C0,C1).
question([what,is | T0],T1,Obj,C0,C1) :-
    noun_phrase(T0,T1,Obj,C0,C1).
question([what | T0],T2,Obj,C0,C2) :-
    noun_phrase(T0,T1,Obj,C0,C1),
    mp(T1,T2,Obj,C1,C2).
question([which | T0],T2,Obj,C0,C2) :-
    noun_phrase(T0,T1,Obj,C0,C1),
    mp(T1,T2,Obj,C1,C2).

% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A,[],C),
    prove_all(C).

% prove_all(L) proves all elements of L against the database
prove_all([]).
prove_all([H|T]) :-
    call(H),
    prove_all(T).

%  The Database of Facts to be Queried

item(vase).
item(painting).

bidder(annie).
bidder(natalie).
bidder(pete).
bidder(bob).
bidder(annie,painting).
bidder(annie,vase).
% bidder(natalie,vase).

starting_price(vase,500).
starting_price(painting,4500).

bidding_price(vase,650).
bidding_price(painting,6000).

bought(Bidder,A):- bidding_price(Obj,A), bidder(Bidder,Obj).
profit(A,B):- starting_price(A, C), bidding_price(A, D), B is D-C.

% YOU CAN ALSO BID AGAINST THE CURRENT BIDDING PRICE OF AN ITEM WITH
% bid(Obj,Val) will return true if your bid overrides the current bid on you item
bid(Obj, Val):- bidding_price(Obj,A), Val-A>0.

% ask([what,is,a,item],A).
% ask([what,is,a,bidder],A).
% ask([what,is,the,bidding,price,of,the,painting],A).
% ask([what,is,the,profit,of,the,vase],A).
% ask([which,bidder,bought,the,painting],A).
% ask([which,item,is,bought,by,annie],A).
% bid(vase,1000).



