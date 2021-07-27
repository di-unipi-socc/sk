%tagAllComponents(ListOf(componentId, LabelPair))
tagAllComponents(TaggedComponents):-
    findall((CId,Pair), (componentSW(CId,_,_,_), tagComponent(CId,Pair)), TaggedComponents),
    findall((CId,Pair), (componentHW(CId,_,_,_), tagComponent(CId,Pair)), TaggedComponents).

tagAllAppComponents(AppId, TaggedComponents):-
    application(AppId, ComponentList),
    findall((CId,Pair), (member(CId, ComponentList), tagComponent(CId,Pair)), TaggedComponents).

tagComponent(ComponentId, (DataSecType,CharactSecType)):-
    componentSW(ComponentId, ListOfData,(ListOfCharact,_),_),
    tagData(ListOfData,ListOfSecTypes),
    highestType(ListOfSecTypes,DataSecType),
    tagCharacteristics(ListOfCharact, ListOfCharactTypes),
    lowestType(ListOfCharactTypes, CharactSecType).
tagComponent(ComponentId, (DataSecType,CharactSecType)):-
    componentHW(ComponentId, ListOfData,ListOfCharact,_),
    tagData(ListOfData,ListOfSecTypes),
    highestType(ListOfSecTypes,DataSecType),
    tagCharacteristics(ListOfCharact, ListOfCharactTypes),
    lowestType(ListOfCharactTypes, CharactSecType),
    checkHwType(ComponentId,DataSecType,CharactSecType).

%tagData(ListOfData,ListOfSecTypes)
tagData([],[]).
tagData([Data|ListOfData],[Type|ListOfSecTypes]):-
    tag(Data,Type),
    tagData(ListOfData, ListOfSecTypes).

%tagCharacteristics(ListOfCharacteristics,ListOfCharacteristicsTypes)
tagCharacteristics([],[HighestType]):-
    highestType(HighestType).
tagCharacteristics([Charact|ListOfCharact],[Type|ListOfCharactTypes]):-
    tag(Charact, Type),
    tagCharacteristics(ListOfCharact, ListOfCharactTypes).

tagCharacteristics([Charact|ListOfCharact],[HighestType|ListOfCharactTypes]):-
    \+(tag(Charact, _)),
    highestType(HighestType),
    tagCharacteristics(ListOfCharact, ListOfCharactTypes).

%check if an hardware component is trusted for the level of its data
checkHwType(_, DataSecType, CharactSecType):-
    maxType(DataSecType, CharactSecType, CharactSecType).
checkHwType(CId, DataSecType, CharactSecType):-
    dif(DataSecType,CharactSecType),
    maxType(DataSecType, CharactSecType, DataSecType),
    componentHW(CId, ListOfData,ListOfCharact,_),
    findall(Param, (member(Param,ListOfData), tag(Param, DataSecType)), Datas),
    findall(Charac, (member(Charac,ListOfCharact), tag(Charac, CharactSecType)), Characs),
    throw(typeError(CId, Characs, CharactSecType, Datas, DataSecType,"hw component security level lower than data security level.")).
