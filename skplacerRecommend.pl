:- ['model.pl'].

skplacer(AppId, NewTags, Partitions) :-
    application(AppId, Hardware, Software),
    suggestionPlacer([],Hardware,Software, NewTags, Partitions). %[] are initial tags

%finds a placement until noerror
%this converge because data types are always decresed and chars type always incresed
suggestionPlacer(Tags,Hardware, Software, Tags, Partitions):-
    placer(Tags, Hardware, Software, Partitions, noerror).
suggestionPlacer(Tags, Hardware, Software, NewerTags, Partitions):-
    placer(Tags, Hardware, Software, _,Error), dif(Error, noerror),
    tagsFromError(Tags, Error, NewTags),
    suggestionPlacer(NewTags, Hardware, Software, NewerTags, Partitions).

%finds the partitions or an error
placer(NewTags, Hardware, Software, Partitions, noerror):-
    hardwareOK(NewTags, Hardware, noerror),
    softwareLabel(NewTags,Software,LabelledSoftware, noerror),
    partition(LabelledSoftware, [], Partitions).
placer(NewTags, Hardware, _, _,Error):-
    hardwareOK(NewTags,Hardware,Error), dif(Error, noerror).
placer(NewTags, Hardware, Software, _,Error):-
    hardwareOK(NewTags,Hardware, noerror),
    softwareLabel(NewTags,Software,_,Error), dif(Error, noerror).

%creates the tags given an error splitting hw and sw errors
tagsFromError(Tags, (Component, DType, CType), NewTags):-
    hardware(Component, Data, Characteristics,_),
    newTags(Tags, Data, Characteristics, DType, CType, NewTags).
tagsFromError(Tags, (Component, DType, CType), NewTags):-
    software(Component,Data,(Characteristics,_),_),
    newTags(Tags, Data, Characteristics, DType, CType, NewTags).

%creates new tags resolving the error
newTags(OldTags, Data, _, _, CType, NewTags):-
    findall((D,T), (member(D,Data), member((D,T),OldTags), lt(CType,T)), OldDataTags),
    findall((D,T), (member(D,Data), \+ member((D,_),OldTags), tag(D,T), lt(CType,T)), OldData),
    append(OldDataTags, OldData, DataToReduce),
    dif(DataToReduce,[]),
    reduceData(DataToReduce,CType, DataReduced),
    mergeTags(OldTags, DataReduced, NewTags).
newTags(OldTags, _, Characteristics, DType, _, NewTags):-
    findall((C,T), (member(C,Characteristics), member((C,T),OldTags), lt(T,DType)), OldCharTags),
    findall((C,T), (member(C,Characteristics), \+ member((C,_),OldTags), tag(C,T), lt(T,DType)), OldChar),
    append(OldCharTags, OldChar, CharToIncrease),
    dif(CharToIncrease,[]),
    increaseChar(CharToIncrease,DType, CharIncreased),
    mergeTags(OldTags, CharIncreased, NewTags).

%reduce the sec type of the data to resolve the error
reduceData([],_,[]).
reduceData([(D,T)|ToReduce], CType, [(D,CType)|Reduced]):-
    lt(CType,T),
    reduceData(ToReduce, CType, Reduced).
reduceData([(_,CType)|ToReduce], CType, Reduced):-
    reduceData(ToReduce, CType, Reduced).
%increase the sec type of the characteristics to resolve the error
increaseChar([],_,[]).
increaseChar([(C,T)|ToIncrease], DType, [(C,DType)|Increased]):-
    lt(T,DType),
    increaseChar(ToIncrease, DType, Increased).
increaseChar([(_,DType)|ToIncrease], DType, Increased):-
    increaseChar(ToIncrease, DType, Increased).

%changes only the modified tags of OldTags
mergeTags(OldTags, TagsModified, NewTags):-
    findall((D,T), (member((D,T),OldTags), \+ member((D,T),TagsModified)), UntouchedOldTags),
    append(TagsModified, UntouchedOldTags, NewTags).

%checks the labelling of hardware components
hardwareOK(NewTags,[H|Hs],Error) :- 
    hardware(H, Data, Characteristics,_),
    dataLabel(NewTags,Data,DLabel), highestType(DLabel,MaxDType),
    characteristicsLabel(NewTags,Characteristics, CLabel), lowestType(CLabel, MinCType),
    gte(MinCType, MaxDType), %check if an hardware component is trusted for the level of its data
    hardwareOK(NewTags,Hs,Error).
hardwareOK(NewTags,[H|_],(H,MaxDType, MinCType)) :- 
    hardware(H, Data, Characteristics,_),
    dataLabel(NewTags,Data,DLabel), highestType(DLabel,MaxDType),
    characteristicsLabel(NewTags,Characteristics, CLabel), lowestType(CLabel, MinCType),
    lt(MinCType,MaxDType).
hardwareOK(_,[],noerror).

%labels software components with Type of Data and  Type of Characteristics
softwareLabel(NewTags,[Sw|Sws],[(Sw,TData,TChar)|LabelledSws],Error):-
    software(Sw,Data,(Characteristics,_),(LinkedHW,_)),
    label(NewTags,Data, Characteristics,(TData,TChar)),
    trustedHW(NewTags, LinkedHW,TData, TChar,noerror),
    softwareLabel(NewTags, Sws,LabelledSws,Error).
softwareLabel(NewTags,[Sw|_],_,HwError):-
    software(Sw,Data,(Characteristics,_),(LinkedHW,_)),
    label(NewTags,Data, Characteristics,(TData,TChar)),
    trustedHW(NewTags, LinkedHW,TData, TChar,HwError),
    dif(HwError,noerror).
softwareLabel(NewTags,[Sw|_],_,(Sw, TData, TChar)):-
    software(Sw,Data,(Characteristics,_),(LinkedHW,_)),
    label(NewTags,Data, Characteristics,(TData,TChar)),
    trustedHW(NewTags, LinkedHW,TData, TChar,HwError),
    dif(HwError,noerror).
softwareLabel(_,[],[],noerror).

%given the list of linked hardware, check if it is trustable with the data
trustedHW(NewTags,LinkedHW, TData, TChar, noerror):-
    \+ (
        lt(TChar,TData),
        member(HW,LinkedHW),hardware(HW,_,Characteristics,_),
        characteristicsLabel(NewTags,Characteristics, CLabels), lowestType(CLabels, MinCType),
        lt(MinCType, TData)
        ).
trustedHW(NewTags,LinkedHW, TData, TChar,(HW, TData, MinCType)):-
        lt(TChar,TData),
        member(HW,LinkedHW),hardware(HW,_,Characteristics,_),
        characteristicsLabel(NewTags,Characteristics, CLabels), lowestType(CLabels, MinCType),
        lt(MinCType, TData).

%given the list of labelled software components, it creates the partitions
partition([(S,TData,TChar)|Ss], Partitions, NewPartitions) :-
    software(S,_,(_,SHW),_),
    partitionCharLabel(TChar,TData,TCP),
    select(((TData,TCP), P, PHW), Partitions, TmpPartitions),
    sumHW(SHW,PHW,NewHW), PNew = ( (TData,TCP), [S|P], NewHW),
    partition(Ss, [PNew|TmpPartitions], NewPartitions).
partition([(S,TData,TChar)|Ss], Partitions, NewPartitions) :-
    software(S,_,(_,SHW),_),
    partitionCharLabel(TChar,TData,TCP),
    \+ member(((TData,TCP), _, _), Partitions), % comment this to find all solutions combinatorially
    P = ( (TData,TCP), [S], SHW),
    partition(Ss, [P|Partitions], NewPartitions).
partition([],P,P).

partitionCharLabel(TChar,TData,safe):- gte(TChar,TData).
partitionCharLabel(TChar,TData,TChar):- lt(TChar,TData).

gte(T1, T2):- T1=T2; (dif(T1,T2), maxType(T1,T2,T1)).
lt(T1, T2):- dif(T1,T2), minType(T1,T2,T1).

sumHW((CPU1, RAM1, HDD1),(CPU2, RAM2, HDD2),(CPU, RAM, HDD)) :-
    CPU is max(CPU1, CPU2),
    RAM is RAM1 + RAM2,
    HDD is HDD1 + HDD2.

label(NewTags, Data,Characteristics, (MaxType,CharactSecType)):-
    dataLabel(NewTags, Data,Label),
    highestType(Label,MaxType),
    characteristicsLabel(NewTags, Characteristics, ListOfCharactTypes),
    lowestType(ListOfCharactTypes, CharactSecType).

%labels a list of data
dataLabel(NewTags,[Data|Ds],[Type|Label]):-
    \+ member((Data,_), NewTags),
    tag(Data,Type),
    dataLabel(NewTags, Ds, Label).
dataLabel(NewTags,[Data|Ds],[Type|Label]):-
    member((Data,Type), NewTags),
    dataLabel(NewTags, Ds, Label).
dataLabel(_,[],[]).
%labels a list of characteristics (empty list is labelled as highest type)
characteristicsLabel(_,[],[HighestType]):-
    highestType(HighestType).
characteristicsLabel(NewTags,[Charact|Characteristics],[Type|ListOfCharactTypes]):-
    \+ member((Charact,_),NewTags),
    tag(Charact, Type),
    characteristicsLabel(NewTags,Characteristics, ListOfCharactTypes).
characteristicsLabel(NewTags,[Charact|Characteristics],[Type|ListOfCharactTypes]):-
    member((Charact,Type),NewTags),
    characteristicsLabel(NewTags,Characteristics, ListOfCharactTypes).
characteristicsLabel(NewTags,[Charact|Characteristics],[HighestType|ListOfCharactTypes]):-
    \+ tag(Charact, _), \+ member((Charact,_),NewTags),
    highestType(HighestType),
    characteristicsLabel(NewTags, Characteristics, ListOfCharactTypes).

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