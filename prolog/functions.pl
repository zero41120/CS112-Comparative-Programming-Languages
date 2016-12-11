
not(X) :- X, !, fail.
not(_).


%%               %%
%  MATH FUNCTION  %
%%               %%
haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1, Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 6371.

getDistance(A, B, Distance) :-
   airport(A, _, Alatdm, Alondm), airport(B, _, Blatdm, Blondm),
   degmin2rad(Alatdm, Alat), degmin2rad(Blatdm, Blat),
   degmin2rad(Alondm, Alon), degmin2rad(Blondm, Blon),
   haversine_radians(Alat, Alon, Blat, Blon, Distance).

degmin2rad( degmin( Degrees, Minutes ), Out) :-
   D is Degrees + Minutes / 60.0,
   Out is D * 0.01745329.

addTime(time(IH, IM), Hour, time(OH, OM)) :-
   R is IH * 60 + IM + Hour * 60,
   OH is floor(R / 60), OM is mod(floor(R), 60).

greaterTime(time(AH, AM), time(BH, BM)) :-
   Z is (AH * 60.0) + AM, 
   B is (BH * 60.0) + BM, 
   not(Z < B).

sumList([], Sum) :- Sum is 0.0.

sumList([Car|Cdr], Sum) :- sumList(Cdr, X), Sum is Car + X.

%%               %%
%  DATA PROVIDER  %
%%               %%
dataProvider(A, B, DepartTime, ArrivalTime, Hour) :-
   flight(A, B, DepartTime),
   getDistance(A, B, D),
   Hour is D / 804.672,
   addTime(DepartTime, Hour, ArrivalTime).

totalTime([flight(A, B, DT) | List], Length) :-
   length(List, 0),  dataProvider(A, B, DT, _, Hour), Length is Hour.

totalTime([flight(A, B, DT) | List], Length) :-
   length(List, L), L > 0, totalTime(flight(A, B, DT), List, Length).
   
totalTime(flight(_, _, time(H, M)), 
         [flight(LA, LB, LDT) | List], Length) :-
   length(List, 0), 
   dataProvider(LA, LB, LDT, time(LH, LM), _),
   Length is (LH * 60 + LM) - (H * 60 + M).

totalTime(flight(A, B, DT), [_|List], Length) :-
   length(List, L), L > 0, totalTime(flight(A, B, DT), List, Length).

%%                %%
%  PRINT FOMRAT    %
%%                %%
pList([]).
pList([Car|Cdr]) :- write(Car), write(' '), pList(Cdr).
toUp([]).
toUp([H|T]):- char_code(C, H), lower_upper(C, U), write(U), toUp(T).
pUpper(In):- atom_codes(In, List), toUp(List).
pTime(time(H, M)):- pad(H), write(':'), pad(M).
pad(A):- A  < 10, format('0~w', [A]).
pad(A):- A >= 10, format('~w' , [A]).


%%                %%
%  PRINT PATH      %
%%                %%
writePath( [] ) :- nl.
writePath( [flight(A,B,DT)|List]) :-
   airport(A, AName, _, _), airport(B, BName, _, _),
   dataProvider(A, B, DT, AT, _),
   write('depart '), pUpper(A), pList(['', AName]), pTime(DT), nl,
   write('arrive '), pUpper(B), pList(['', BName]), pTime(AT), nl,
   writePath(List).

%%                %%
%  CONDITION       %
%%                %%
checkTransition(TimeA, TimeB):-
   addTime(TimeA, 0.5, TransA),
   greaterTime(TimeB, TransA).

checkSameDay(flight(A, B, DT), TimeList):-
   dataProvider(A, B, DT, _, Hour),
   sumList(TimeList, Total), 
   (Total + Hour) < 24.

checkMember(_, []).
checkMember(M, [flight(C, A, _)| Cdr]) :-
   not( M = C ), not( M = A ), checkMember(M, Cdr).


%%                %%
%  SHORTEST PATH   %
%%                %%

getShortestPath(A, B, List) :-
   findPath(A, B, List),
   getShorter(A, B, List).

getShorter(A, B, ListA) :-
   findPath(A, B, ListB),
   totalTime(ListA, AHour),
   totalTime(ListB, BHour),
   AHour > BHour,
   !, fail.

getShorter(_, _, _).

%%                %%
%  FIND PATH       %
%%                %%
findPath( A, B, [flight(A, M, DT) | List] ) :-
   findPath( M, B, [flight(A, M, DT)], List, []).

findPath( A, A, _, [], _).
findPath( A, B,
   [flight(LA, LB, LDT)|Tried], 
   [flight(A, M, DT)|List], TimeList) :-
   flight(A, M, DT), 
   dataProvider(LA, LB, LDT, LAT, Hour),
   not(B = LB),
   checkMember(M, Tried ), 
   checkTransition(LAT, DT), 
   checkSameDay(flight(A,M,DT), TimeList),
   Tried2 = [flight(LA,LB,LDT)| Tried],
   findPath( M, B, [flight(A, M, DT)|Tried2], List, [Hour | TimeList]). 

%%      %%
%  FIND  %
%%      %%
%findPath(A, B, List) :- findPath(A, B, [A], List, [], time(0, 0)).
%findPath(A, A, _, [A], _, _). 
%findPath(A, B, Tried, [A|List], TotalHour, InTime) :-
%   addTime(InTime, 0.5, TransTime),
%   dataProvider(A, M, Hour, DepartTime, ArrivalTime),
%   not(greaterTime(TransTime, DepartTime)),
%   not(member( M, Tried)),
%   sumList(TotalHour, G), 
%   G < 24,
%   findPath( M, B, [M|Tried], List, [Hour|TotalHour], ArrivalTime).


%%             %%
%  MAIN         %
%%             %%

%fly(A, B) :- 
%   findPath(A, B, List), nl, 
%   writePath(List).
%fly(A, B) :- 
%   not(findPath(A, B, _)), 
%   write('Error: No such path.'), !, fail.

fly(A, B) :- 
   getShortestPath(A, B, List), nl, 
   writePath(List), !, fail.

fly(A, B) :- 
   not(getShortestPath(A, B, _)), 
   write('Error: No such path.'), !, fail.

fly(A, A) :- 
   airport(A, _, _, _),    write('Notice: Same airport.')    , !, fail.
fly(A, _) :- 
   not(airport(A, _,_,_)), write('Error: Departure invalid.'), !, fail.
fly(_, A) :- 
   not(airport(A, _,_,_)), write('Error: Arrival invalid')   , !, fail.
