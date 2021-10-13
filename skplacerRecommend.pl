:- ['model.pl', 'utils.pl'].

sKnife(AppId, NewTags, Partitioning) :-
    application(AppId, Hardware, Software),
    eligiblePartitioning(Hardware, Software, NewTags, Partitioning). 

eligiblePartitioning(H,S,T,P) :- eligiblePartitioning([],H,S,T,P).

%finds an eligiblePartition until ok
%converges since data types are always decreased and char types always increased
eligiblePartitioning(Tags,Hardware, Software, Tags, Partitioning):-
    partitioningResult(Tags, Hardware, Software, Partitioning, ok).
eligiblePartitioning(Tags, Hardware, Software, NewTags, Partitioning):-
    partitioningResult(Tags, Hardware, Software, _,ko(E, DT, CT)),
    tagsOK(Tags, ko(E, DT, CT), TmpTags),
    eligiblePartitioning(TmpTags, Hardware, Software, NewTags, Partitioning).

%finds the partitions or an error
partitioningResult(NewTags, Hardware, Software, Partitions, ok):-
    hardwareOK(NewTags, Hardware, ok),
    softwareLabel(NewTags,Software,LabelledSoftware, ok),
    partitioning(LabelledSoftware, [], Partitions).
partitioningResult(NewTags, Hardware, _, _,ko(H, DT, CT)):-
    hardwareOK(NewTags,Hardware, ko(H, DT, CT)).
partitioningResult(NewTags, Hardware, Software, _,ko(E, DT, CT)):-
    hardwareOK(NewTags,Hardware, ok),
    softwareLabel(NewTags,Software,_,ko(E, DT, CT)).

%checks the labelling of hardware components
hardwareOK(NewTags,[H|Hs],Error) :- 
    hardware(H, Data, Characteristics,_),
    dataLabel(NewTags,Data,DLabel), highestType(DLabel,MaxDType),
    characteristicsLabel(NewTags,Characteristics, CLabel), lowestType(CLabel, MinCType),
    gte(MinCType, MaxDType), %check if an hardware component is trusted for the level of its data
    hardwareOK(NewTags,Hs,Error).
hardwareOK(NewTags,[H|_], ko(H,MaxDType, MinCType)) :- 
    hardware(H, Data, Characteristics,_),
    dataLabel(NewTags,Data,DLabel), highestType(DLabel,MaxDType),
    characteristicsLabel(NewTags,Characteristics, CLabel), lowestType(CLabel, MinCType),
    lt(MinCType,MaxDType).
hardwareOK(_,[],ok).

%labels software components with Type of Data and  Type of Characteristics
softwareLabel(NewTags,[Sw|Sws],[(Sw,TData,TChar)|LabelledSws],Error):-
    software(Sw,Data,(Characteristics,_),(LinkedHW,_)),
    label(NewTags,Data, Characteristics,(TData,TChar)),
    trustedHW(NewTags, LinkedHW,TData, TChar,ok),
    softwareLabel(NewTags, Sws,LabelledSws,Error).
softwareLabel(NewTags,[Sw|_],_,HwError):-
    software(Sw,Data,(Characteristics,_),(LinkedHW,_)),
    label(NewTags,Data, Characteristics,(TData,TChar)),
    trustedHW(NewTags, LinkedHW,TData, TChar,HwError),
    dif(HwError,ok).
softwareLabel(NewTags,[Sw|_],_,ko(Sw, TData, TChar)):-
    software(Sw,Data,(Characteristics,_),(LinkedHW,_)),
    label(NewTags,Data, Characteristics,(TData,TChar)),
    trustedHW(NewTags, LinkedHW,TData, TChar,HwError),
    dif(HwError,ok).
softwareLabel(_,[],[],ok).

%given the list of linked hardware, check if it is trustable with the data
trustedHW(NewTags, LinkedHW, TData, TChar, ok):-
    \+ (
        lt(TChar,TData),
        member(HW,LinkedHW),hardware(HW,_,Characteristics,_),
        characteristicsLabel(NewTags,Characteristics, CLabels), lowestType(CLabels, MinCType),
        lt(MinCType, TData)
        ).
trustedHW(NewTags, LinkedHW, TData, TChar, ko(HW,TData,MinCType)):-
        lt(TChar,TData),
        member(HW,LinkedHW),hardware(HW,_,Characteristics,_),
        characteristicsLabel(NewTags,Characteristics, CLabels), lowestType(CLabels, MinCType),
        lt(MinCType, TData).

%given the list of labelled software components, it creates the partitions
partitioning([(S,TData,TChar)|Ss], Partitions, NewPartitions) :-
    software(S,_,(_,SHW),_),
    characteristicLabel(TChar,TData,TCP),
    select(((TData,TCP), P, PHW), Partitions, TmpPartitions),
    sumHW(SHW,PHW,NewHW), PNew = ( (TData,TCP), [S|P], NewHW),
    partitioning(Ss, [PNew|TmpPartitions], NewPartitions).
partitioning([(S,TData,TChar)|Ss], Partitions, NewPartitions) :-
    software(S,_,(_,SHW),_),
    characteristicLabel(TChar,TData,TCP),
    \+ member(((TData,TCP), _, _), Partitions), % comment this to find all solutions combinatorially
    P = ( (TData,TCP), [S], SHW),
    partitioning(Ss, [P|Partitions], NewPartitions).
partitioning([],P,P).

% assigns the suitable label to a characterstic in the partition
characteristicLabel(TChar,TData,safe):- gte(TChar,TData).
characteristicLabel(TChar,TData,TChar):- lt(TChar,TData).

%creates the tags given an error splitting hw and sw errors
tagsOK(Tags, ko(Component, DType, CType), NewTags):-
    hardware(Component, Data, Characteristics,_),
    tagsOK(Tags, Data, Characteristics, DType, CType, NewTags).
tagsOK(Tags, ko(Component, DType, CType), NewTags):-
    software(Component,Data,(Characteristics,_),_),
    tagsOK(Tags, Data, Characteristics, DType, CType, NewTags).

%creates new tags resolving the error
tagsOK(OldTags, Data, _, _, CType, NewTags):-
    findall((D,T), (member(D,Data), member((D,T),OldTags), lt(CType,T)), OldDataTags),
    findall((D,T), (member(D,Data), \+ member((D,_),OldTags), tag(D,T), lt(CType,T)), OldData),
    append(OldDataTags, OldData, DataToReduce),
    dif(DataToReduce,[]),
    reduceData(DataToReduce,CType, DataReduced),
    mergeTags(OldTags, DataReduced, NewTags).
tagsOK(OldTags, _, Characteristics, DType, _, NewTags):-
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