:- use_module(library(apply)).
:- use_module(library(csv)).

get_rows_data(File, Lists):-
csv_read_file(File, Rows, []),
rows_to_lists(Rows, Lists).

rows_to_lists(Rows, Lists):-
maplist(row_to_list, Rows, Lists).

row_to_list(Row, List):-
Row =.. [row|List].

% get_bidder([[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]],R).
% bidder
get_bidder([_|T],Result):-
get_bidder1(T,Result).

get_bidder1([],[]).

%get_bidder1([L],[Elem]) :-
%nth1(4, L , Elem).

get_bidder1([H|T],[Elem|Rest]) :-
nth1(4, H , Elem),
dif([],H),
get_bidder1(T,Rest).



result_bidder(R) :- get_rows_data("data.csv", Lists),get_bidder(Lists,R).

% price
get_price([_|T],Result):-
get_price1(T,Result).

%get_price1([L],[Elem]) :-
%nth1(7, L , Elem).

get_price1([],[]).

get_price1([H|T],[Elem|Rest]) :-
nth1(7, H , Elem),
dif([],H),
get_price1(T,Rest).


result_price(R) :- get_rows_data("data.csv", Lists),get_price(Lists,R).

% item
get_item([_|T],Result):-
get_item1(T,Result).

%get_item1([L],[Elem]) :-
%nth1(1, L , Elem).

get_item1([],[]).

get_item1([H|T],[Elem|Rest]) :-
nth1(1, H , Elem),
%dif([],H),
get_item1(T,Rest).

result_item(R) :- get_rows_data("data.csv", Lists),get_item(Lists,R).

% start
get_start([_|T],Result):-
get_start1(T,Result).

get_start1([],[]).
%get_start1([L],[Elem]) :-
%nth1(1, L , Elem).

get_start1([H|T],[Elem|Rest]) :-
nth1(1, H , Elem),
dif([],H),
get_start1(T,Rest).

result_start(R) :- get_rows_data("data.csv", Lists),get_start(Lists,R).


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
mp([did|T0],T2,O1,C0,C2) :-
reln(T0,T1,O1,O2,C0,C1),
noun_phrase(T1,T2,O2,C1,C2).

% DICTIONARY

adjective([Lang,speaking | T],T,Obj,C,[language(Obj,Lang)|C]).

noun([item | T],T,Obj,C,[item(Obj)|C]).
noun([bidder | T],T,Obj,C,[bidder(Obj)|C]).
noun([X | T],T,X,C,C) :- item(X).
noun([X | T],T,X,C,C) :- bidder(X).

reln([the,starting,price,of | T],T,O1,O2,C,[starting_price(O2,O1)|C]).
reln([the,bidding,price,of | T],T,O1,O2,C,[bidding_price(O2,O1)|C]).
reln([the,profit,of | T],T,O1,O2,C,[profit(O2,O1)|C]).
reln([buy | T],T,O1,O2,C,[buy(O1,O2)|C]).

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
item(A) :-
member(A,R),
result_item(R).

bidder(annie).
bidder(natalie).
bidder(pete).
bidder(bob).
bidder(A) :-
member(A,R),
result_bidder(R).

bidder(annie,vase).
bidder(natalie,painting).
bidder(A,P,N) :-
nth1(N,Bidder_list,A),
nth1(N,Item_list,P),
length(Bidder_list,L),
N<L+1,
result_bidder(Bidder_list),
result_item(Item_list).


starting_price(vase,500).
starting_price(painting,4500).
starting_price(O,P) :-
nth1(N,Item_list,O),
nth1(N,Price_list,P),
result_item(Item_list),
result_start(Price_list).

bidding_price(vase,650).
bidding_price(painting,6000).
bidding_price(O,P) :-
nth1(N,Item_list,O),
nth1(N,Price_list,P),
result_item(Item_list),
result_price(Price_list).

bought(Bidder,A):- bidding_price(Obj,A), bidder(Bidder,Obj).
profit(A,B):- starting_price(A, C), bidding_price(A, D), B is D-C.

% YOU CAN ALSO BID YOURSELF AGAINST THE CURRENT BIDDING PRICE OF AN ITEM
bid(Obj, Val):- bidding_price(Obj,A), Val-A>0.

% ask([what,is,a,item],A).
% ask([what,is,a,bidder],A).
% ask([what,is,the,bidding,price,of,a,painting],A).
% ask([what,is,the,profit,of,a,vase],A).
% ask([which,bidder,bought,a,painting],A).
% ask([which,item,is,bought,by,annie],A).
% bid(vase,1000).









