:- ['model.pl'].

sKnife(AppId, Partitions) :-
    application(AppId, Hardware, Software),
    hardwareOK(Hardware),
    softwareLabel(Software, LabelledSoftware),
    softwareOk(LabelledSoftware),
    partitioning(LabelledSoftware, [], Partitions).

%check the labelling of hardware components
hardwareOK([H|Hs]) :- 
    hardware(H, Data, Characteristics,_),
    labelC(Data, Characteristics, (TData,TChar)),
    gte(TChar,TData), %check if an hardware component is trusted for the level of its data 
    hardwareOK(Hs).
hardwareOK([]).

%labels software components with Type of Data and  Type of Characteristics
softwareLabel([Sw|Sws],[(Sw,TData,TChar)|LabelledSws]):-
    software(Sw, Data,Characteristics,_),
    labelC(Data, Characteristics,(TData,TChar)),
    softwareLabel(Sws,LabelledSws).
softwareLabel([],[]).

softwareOk(LabelledSoftware):-
    \+ (
        member((Sw,TData,TChar), LabelledSoftware),
        lt(TChar,TData),
        externalLeak([Sw], [] ,TData, LabelledSoftware)
      ).

externalLeak(LinkedSW, Visited,TData, LabelledSoftware):-
    member(Sw, LinkedSW), \+(member(Sw, Visited)), member((Sw,_,TChar), LabelledSoftware),
    lt(TChar,TData),
    software(Sw, _,_,(LinkedHW,_)),
    \+ trustedHW(LinkedHW, TData).

externalLeak(LinkedSW, Visited,TData, LabelledSoftware):-
    member(Sw, LinkedSW), \+(member(Sw, Visited)), member((Sw,_,TChar), LabelledSoftware),
    lt(TChar,TData),
    software(Sw, _,_,(LinkedHW,VisitLinkedSW)),
    trustedHW(LinkedHW, TData),
    externalLeak(VisitLinkedSW, [Sw|Visited], TData, LabelledSoftware).

%given the list of linked hardware, check if it is trustable with the data
trustedHW(LinkedHW, TData):-
    \+ (
        member(HW,LinkedHW),hardware(HW,_,Characteristics,_),
        characteristicsLabel(Characteristics, CLabel), lowestType(CLabel, MinCType),
        lt(MinCType, TData)
        ).

partitioning([(S,TData,TChar)|Ss], Partitioning, NewPartitioning) :-
    partitionCharLabel(TChar,TData,TCD),
    select( ((TData,TCD), Ds), Partitioning, TmpPartitioning),
    DNew = ( (TData,TCD), [S|Ds]),
    partitioning(Ss, [DNew|TmpPartitioning], NewPartitioning).
partitioning([(S,TData,TChar)|Ss], Partitioning, NewPartitioning) :-
    partitionCharLabel(TChar,TData,TCD),
    \+ member( ((TData,TCD), _), Partitioning), % comment this to find all solutions combinatorially
    DNew = ( (TData,TCD), [S]),
    partitioning(Ss, [DNew|Partitioning], NewPartitioning).
partitioning([],P,P).

partitionCharLabel(TChar,TData,safe):- gte(TChar,TData).
partitionCharLabel(TChar,TData,TChar):- lt(TChar,TData).


gte(T1, T2):- T1=T2; (dif(T1,T2), maxType(T1,T2,T1)).
lt(T1, T2):- dif(T1,T2), minType(T1,T2,T1).

sumHW((CPU1, RAM1, HDD1),(CPU2, RAM2, HDD2),(CPU, RAM, HDD)) :-
    CPU is max(CPU1, CPU2),
    RAM is RAM1 + RAM2,
    HDD is HDD1 + HDD2.

labelC(Data, Characteristics, (MaxType,CharactSecType)):-
    dataLabel(Data,Label),
    highestType(Label,MaxType),
    characteristicsLabel(Characteristics, ListOfCharactTypes),
    lowestType(ListOfCharactTypes, CharactSecType).

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
    \+ tag(Charact, _),
    highestType(HighestType),
    characteristicsLabel(Characteristics, ListOfCharactTypes).

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
highestType(HighestType):- g_lattice_higherThan(HighestType,_), \+ (g_lattice_higherThan(_,HighestType)).

%lowest type of the lattice
lowestType(LowestType):- g_lattice_higherThan(_,LowestType), \+ (g_lattice_higherThan(LowestType,_)).