:- ['model.pl'].

partition(AppId, Partitions) :-
    application(AppId, Hardware, Software),
    hardwareOK(Hardware),
    partition(Software, [], Partitions).

hardwareOK([H|Hs]) :- 
    hardware(H, Data, Characteristics,_),
    dataLabel(Data,DLabel), highestType(DLabel,MaxDType),
    characteristicsLabel(Characteristics, CLabel), lowestType(CLabel, MaxCType),
    checkHwType(H,MaxDType,MaxCType),
    hardwareOK(Hs).
hardwareOK([]).

partition([S|Ss], Partitions, NewPartitions) :-
    software(S,_,(_,SHW),_), label(S, (TData,TChar)), gt(TChar,TData),
    member( ((TData,TChar1), P, PHW), Partitions), 
    select( ((TData,TChar1), P, PHW), Partitions, TmpPartitions),
    sumHW(SHW,PHW,NewHW), PNew = ( (TData,TChar1), [S|P], NewHW),
    partition(Ss, [PNew|TmpPartitions], NewPartitions).
partition([S|Ss], Partitions, NewPartitions) :-
    software(S,_,(_,SHW),LinkedC), label(S, (TData,TChar)), \+ gt(TChar,TData),
    select( ((TData,TChar), P, PHW), Partitions, TmpPartitions),
    sumHW(SHW,PHW,NewHW), PNew = ( (TData,TChar), [S|P], NewHW),
    %TODO: check hardware che tocca
    partition(Ss, [PNew|TmpPartitions], NewPartitions).
partition([S|Ss], Partitions, NewPartitions) :-
    software(S,_,(_,SHW),_), label(S, (TData,TChar)),
    \+ member( ((TData,TChar), _, _), Partitions), % comment this to find all solutions combinatorially
    P = ( (TData,TChar), [S], SHW),
    partition(Ss, [P|Partitions], NewPartitions). 
partition([],P,P).

gte(T,T).
gte(T1, T2):- dif(T1,T2), maxType(T1,T2,T1).

gt(T1, T2):- dif(T1,T2), maxType(T1,T2,T1).

sumHW((CPU1, RAM1, HDD1),(CPU2, RAM2, HDD2),(CPU, RAM, HDD)) :-
    CPU is max(CPU1, CPU2),
    RAM is RAM1 + RAM2,
    HDD is HDD1 + HDD2.

label(H, (MaxType,CharactSecType)):-
    software(H, Data,(Characteristics,_),_),
    dataLabel(Data,Label),
    highestType(Label,MaxType),
    characteristicsLabel(Characteristics, ListOfCharactTypes),
    lowestType(ListOfCharactTypes, CharactSecType).
/*label(H, (MaxType,CharactSecType)):-
    hardware(H, Data,Characteristics,_),
    dataLabel(Data,Label),
    highestType(Label,MaxType),
    characteristicsLabel(Characteristics, ListOfCharactTypes),
    lowestType(ListOfCharactTypes, CharactSecType),
    checkHwType(H,MaxType,CharactSecType).*/

%tagData(Data,Label)
dataLabel([Data|Ds],[Type|Label]):-
    tag(Data,Type),
    dataLabel(Ds, Label).
dataLabel([],[]).
%tagCharacteristics(ListOfCharacteristics,ListOfCharacteristicsTypes)
characteristicsLabel([],[HighestType]):-
    highestType(HighestType).
characteristicsLabel([Charact|Characteristics],[Type|ListOfCharactTypes]):-
    tag(Charact, Type),
    characteristicsLabel(Characteristics, ListOfCharactTypes).
characteristicsLabel([Charact|Characteristics],[HighestType|ListOfCharactTypes]):-
    \+(tag(Charact, _)),
    highestType(HighestType),
    characteristicsLabel(Characteristics, ListOfCharactTypes).

%check if an hardware component is trusted for the level of its data
checkHwType(_, MaxType, CharactSecType):-
    maxType(MaxType, CharactSecType, CharactSecType).
checkHwType(CId, MaxType, CharactSecType):-
    dif(MaxType,CharactSecType),
    maxType(MaxType, CharactSecType, MaxType),
    hardware(CId, Data,Characteristics,_),
    findall(Param, (member(Param,Data), tag(Param, MaxType)), Datas),
    findall(Charac, (member(Charac,Characteristics), tag(Charac, CharactSecType)), Characs),
    throw(typeError(CId, Characs, CharactSecType, Datas, MaxType,"hw component security level lower than data security level.")).

%find highest type in a list
highestType([T], T).
highestType([T1,T2|Ts], MaxT) :-
	highestType([T2|Ts], MaxTofRest),
	maxType(T1, MaxTofRest, MaxT).

%find max between two types
maxType(T1,T2,TMax):- once(innerMaxType(T1,T2,TMax)).
innerMaxType(X, X, X).
innerMaxType(X, Y, X) :- dif(X,Y), lattice_higherThan(X,Y).
innerMaxType(X, Y, Y) :- dif(X,Y), lattice_higherThan(Y,X).
innerMaxType(X, Y, Top) :-											%labels not reachable with path (on different branch) 
	dif(X,Y), \+ lattice_higherThan(X,Y), \+ lattice_higherThan(Y,X),
	lattice_higherThan(Top, X), lattice_higherThan(Top, Y),
	\+ (lattice_higherThan(Top, LowerTop), lattice_higherThan(LowerTop, X), lattice_higherThan(LowerTop, Y)).

%find lowest type in a list
lowestType([T], T).
lowestType([T1,T2|Ts], MinT) :-
	lowestType([T2|Ts], MinOfRest),
	minType(T1, MinOfRest, MinT).

%find min between two types
minType(T1,T2,TMin):- once(innerMinType(T1,T2,TMin)).
innerMinType(X, X, X).
innerMinType(X, Y, X) :- dif(X,Y), lattice_higherThan(Y,X).
innerMinType(X, Y, Y) :- dif(X,Y), lattice_higherThan(X,Y).
innerMinType(X, Y, Low) :-											%labels not reachable with path (on different branch) 
	dif(X,Y), \+ lattice_higherThan(X,Y), \+ lattice_higherThan(Y,X),
	lattice_higherThan(X,Low), lattice_higherThan(Y,Low),
	\+ (lattice_higherThan(HigherLow, Low), lattice_higherThan(X,HigherLow), lattice_higherThan(Y,HigherLow)).

%navigate the lattice    
lattice_higherThan(X, Y) :- g_lattice_higherThan(X,Y).
lattice_higherThan(X, Y) :- g_lattice_higherThan(X,W), lattice_higherThan(W,Y).



%highestType of the lattice
highestType(HighestType):-
    g_lattice_higherThan(HighestType,_), \+ (g_lattice_higherThan(_,HighestType)).

%lowest type of the lattice
lowestType(LowestType):-
	g_lattice_higherThan(_,LowestType), \+ (g_lattice_higherThan(LowestType,_)).