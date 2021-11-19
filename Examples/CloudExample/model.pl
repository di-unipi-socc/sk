%application(appId, listOfComponents)
application(iotApp1, [network, disk],[userConfig, appManager, authenticator, aiLearning, apiGateway, db]).
application(iotApp2, [network, disk],[userConfig2, appManager, authenticator, aiLearning2, apiGateway, db]).

%software(componentId, ListOfData,ListOfCharacteristics,[LinkedHW, LinkedSW])
%hardware(componentId, ListOfData,ListOfCharacteristics,[LinkedHW, LinkedSW])
software( userConfig, 
            [userPreferences], %data
            [dataLibrary],% ListOfCharacteristics
            ([],[aiLearning, appManager, authenticator, db]) %linked components (hw,sw)
            ).

software( appManager, 
            [iotMeasurements, userRequests, iotCommands, iotEvents], 
            [],
            ([],[aiLearning, userConfig, authenticator, db]) 
            ).

software( authenticator, 
            [iotMeasurements, userRequests, iotCommands, iotEvents, networkData, userPreferences], 
            [tlsLibrary],
            ([],[userConfig, appManager, apiGateway]) 
            ).

software( aiLearning, 
            [iotMeasurements, userPreferences], 
            [aiFramework], 
            ([],[userConfig, appManager]) 
            ).

software( apiGateway, 
            [networkData], 
            [networkLibrary],
            ([network],[authenticator]) 
            ).

software( db, 
            [iotMeasurements, userPreferences, cryptedData], 
            [dbms],
            ([disk],[userConfig, appManager]) 
            ).

software( userConfig2, 
            [userPreferences], %data
            [dataLibrary],% ListOfCharacteristics
            ([disk],[aiLearning2, appManager, authenticator, db]) %linked components (hw,sw)
            ).
software( aiLearning2, 
            [iotMeasurements, userPreferences], 
            [aiFramework],
            ([],[userConfig2, appManager]) 
            ).
%%%%%Hardware componenents

hardware( network, 
            [networkData], 
            [fromProvider],
            ([],[apiGateway]) 
            ).
hardware( disk, 
            [cryptedData], 
            [fromProvider],
            ([],[db]) 
            ).

hardware( disk2, 
            [cryptedData, userPreferences], 
            [fromProvider],
            ([],[db, userConfig2]) 
            ).

% lattice of security 
%g_lattice_higherThan(higherElement, lowerElement)
g_lattice_higherThan(top, medium).
g_lattice_higherThan(medium, low).

%policies for data
%tag(variable, security level)
tag(networkData, low).
tag(cryptedData, low).
tag(userPreferences, medium).
tag(userRequests, medium).
tag(iotMeasurements, top).
tag(iotEvents, top).
tag(iotCommands, top).

%policies for characteristics
tag(tlsLibrary, top).
tag(fromProvider, low).
tag(aiFramework, low).
tag(dbms, top).
tag(networkLibrary, low).
tag(dataLibrary, medium).