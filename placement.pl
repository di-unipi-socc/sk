:- use_module(library(lists)).
:- consult('model').
:- consult('utils').
:- consult('tagging').


%createPartitions(ComponentList, Placement)
%Placement = listOf[ p(DatySecType, optional(CharSecType)), ComponentList, (TotalCPUs, TotalMemory, TotalStorage)  ]
placeOnSK(AppId, Placement):-
    catch(tagAllAppComponents(AppId, TaggedComponents),
        typeError(CId, Charas, CharType, Datas, DataType, String),
        (writef('%w %w (%w:%w < %w:%w)\n',[CId,String, Charas, CharType, Datas, DataType]),fail)),
    createPartitions(TaggedComponents, Partitions),
    placeComponents(TaggedComponents, Partitions, Placement).

%createPartitions(TaggedComponents, Partitions).
%create partitions based on the tagged componenents: 
%one per data level (if higher than char. level), one per pair (data level, char level), if char. level is higher
createPartitions(TaggedComponents,Partitions):-
    findPartitions(TaggedComponents, AllPartitions),
    sort(AllPartitions, Partitions).
%find every partition with duplicates
%findPartition(TaggedComponents(CId, DataType, CharType), AllPartitions)
/*
findPartitions([],[]).
findPartitions([(_,DataType,CharType)|TaggedComponents], [p(DataType)|Partitions]):-
    maxType(DataType,CharType,CharType),
    findPartitions(TaggedComponents, Partitions).
findPartitions([(_,DataType,CharType)|TaggedComponents], [p(DataType,CharType)|Partitions]):-
    dif(DataType,CharType),
    maxType(DataType,CharType,DataType),
    findPartitions(TaggedComponents, Partitions).*/
%%%%%%
findPartitions(TaggedComponents, Partitions):-
    findall(p(DataType),
            (member((_,DataType,CharType),TaggedComponents), maxType(DataType,CharType,CharType)),
            SinglePartitions),
    findall(p(DataType,CharType),
           (member((_,DataType,CharType),TaggedComponents), maxType(DataType,CharType,DataType), dif(DataType,CharType)),
            PairPartitions),
    append(SinglePartitions,PairPartitions, Partitions).

/*
findPartitions([(_,DataType,CharType)|TaggedComponents],[ThisPartition|Partitions]):-
    maxType(DataType,CharType,MaxType),
    findThisPartition(CharType, MaxType, ThisPartition),
    findPartitions(TaggedComponents, Partitions).

%findThisPartition(CharType, MaxType, CalculatedPartition).
findThisPartition(CharType, CharType, p(CharType)).
findThisPartition(CharType, MaxType, p(MaxType, CharType)):- dif(CharType,MaxType).
*/



%placeComponents(TaggedComponents, Partitions, Placement)
%place every sw component in the right partition
placeComponents(_,[],[]).
placeComponents(TaggedComponents, [p(DataType,CharType)|Partitions], [p((DataType,CharType),Partition, Hw)|Placement]):-
    findall(CId, (member((CId,DataType,CharType),TaggedComponents),componentSW(CId,_,_,_)), Partition),
    partitionHw(Partition, Hw),
    placeComponents(TaggedComponents, Partitions, Placement).
placeComponents(TaggedComponents, [p(Type)|Partitions], [(p(Type),Partition, Hw)|Placement]):-
    findall(CId, 
        (member((CId,Type,CharType),TaggedComponents), maxType(Type,CharType,CharType),componentSW(CId,_,_,_)), 
        Partition),
    partitionHw(Partition, Hw),
    placeComponents(TaggedComponents, Partitions, Placement).

%calculate the hardware usage of each partition
partitionHw([],(0,0,0)).
partitionHw([CId|Partition],(NewCPU, NewMem, NewStor)):-
    partitionHw(Partition, (OldCPU, OldMem, OldStor)),
    componentSW(CId,_,(_,(CPU,Mem,Stor)),_),
    NewCPU is max(CPU, OldCPU),
    NewMem is Mem + OldMem,
    NewStor is Stor + OldStor.