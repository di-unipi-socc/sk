%application(appId, listOfComponents)
application(iotApp1, [userConfig, businessLogic, authentication, aiLearning, network1Controller, 
                      network2Controller, thermostatController, lightsController, cameraController, lockController,
                      diskController, network1, network2, thermostat, lights, camera, lock, disk]).
application(iotApp2, [userConfig, businessLogic, authentication, aiLearning, network1Controller, 
                      network2Controller, thermostatController, lightsController, cameraController, lockController,
                      diskController, network1, network2, thermostat2, lights, camera, lock, disk]).
application(iotApp3, [userConfig, businessLogic, authentication, aiLearning, network1Controller, 
                      network2Controller, thermostatController, lightsController, cameraController, lockController,
                      diskController2, disk, diskDeclassifier, network1, network2, thermostat, lights, camera, 
                      lock, disk]).

%componentSW(componentId, ListOfData,ListOfReqs,ListOfLinkedComponents,SW)
%componentHW(componentId, ListOfData,ListOfCharacteristics,ListOfLinkedComponents)
componentSW( userConfig, 
            [wantedTemp, wantedBright, cameraActivation, schedule, aiActive], %data
            ([], (2,256,128)), %reqs(ListOfCharacteristics, (vCPUs, MBmemory,MBstorage))
            [aiLearning, businessLogic, authentication, diskController] %linked components
            ).

componentSW( businessLogic, 
            [wantedTemp, wantedBright, cameraActivation, schedule, aiActive, thermostatTemp, lightBright,
                cameraStream, thermostatStatus, lightStatus, cameraStatus, lockStatus, lockCommands,
                actuationCommands, cameraLive], %data
            ([], (1,512,0)), %reqs(ListOfCharacteristics, (vCPUs, MBmemory,MBstorage))
            [aiLearning, businessLogic, authentication] %linked components
            ).

componentSW( authentication, 
            [wantedTemp, wantedBright, cameraActivation, schedule, aiActive, thermostatTemp, lightBright,
                cameraStream, thermostatStatus, lightStatus, cameraStatus, lockStatus, lockCommands,
                actuationCommands, cameraLive, credentials], %data
            ([tlsLibrary], (1,512,256)), %reqs(ListOfCharacteristics, (vCPUs, MBmemory,MBstorage))
            [userConfig, businessLogic, network1Controller, diskController] %linked components
            ).

componentSW( thermostatController, 
            [wantedTemp, thermostatTemp, actuationCommands], %data
            ([], (1,512,0)), %reqs(ListOfCharacteristics, (vCPUs, MBmemory,MBstorage))
            [businessLogic, network2Controller] %linked components
            ).

componentSW( lightsController, 
            [wantedBright, lightBright, actuationCommands], %data
            ([], (1,512,0)), %reqs(ListOfCharacteristics, (vCPUs, MBmemory,MBstorage))
            [businessLogic, network2Controller] %linked components
            ).

componentSW( cameraController, 
            [cameraActivation, cameraLive, cameraStatus, cameraStream], %data
            ([], (1,512,0)), %reqs(ListOfCharacteristics, (vCPUs, MBmemory,MBstorage))
            [businessLogic, network2Controller] %linked components
            ).

componentSW( lockController, 
            [lockCommands, lockStatus], %data
            ([], (1,512,0)), %reqs(ListOfCharacteristics, (vCPUs, MBmemory,MBstorage))
            [businessLogic, network2Controller] %linked components
            ).

componentSW( network1Controller, 
            [networkInput, networkOutput], %data
            ([networkDriver], (1,512,0)), %reqs(ListOfCharacteristics, (vCPUs, MBmemory,MBstorage))
            [authentication, network1] %linked components
            ).

componentSW( network2Controller, 
            [wantedTemp, wantedBright, cameraActivation, thermostatTemp, lightBright,
                cameraStream, thermostatStatus, lightStatus, cameraStatus, lockStatus, lockCommands,
                actuationCommands, cameraLive], %data
            ([], (1,512,256)), %reqs(ListOfCharacteristics, (vCPUs, MBmemory,MBstorage))
            [thermostatController, lightsController, lockController, cameraController, network2, diskController] %linked components
            ).

componentSW( diskController, 
            [schedule, credentials, wantedTemp, wantedBright, cameraActivation, aiActive], %data
            ([], (1,512,0)), %reqs(ListOfCharacteristics, (vCPUs, MBmemory,MBstorage))
            [userConfig, authentication, network2Controller, disk] %linked components
            ).

componentSW( aiLearning, 
            [wantedTemp, wantedBright, cameraActivation, schedule, aiActive,thermostatTemp, lightBright, cameraStream], %data
            ([aiFramework], (4,2048,0)), %reqs(ListOfCharacteristics, (CPU, Memory,Storage))
            [userConfig, businessLogic] %linked components
            ).

componentSW( diskController2, 
            [schedule, credentials, wantedTemp, wantedBright, cameraActivation, aiActive], %data
            ([diskDriver], (1,512,0)), %reqs(ListOfCharacteristics, (vCPUs, MBmemory,MBstorage))
            [businessLogic, diskDeclassifier] %linked components
            ).

componentSW( diskDeclassifier, 
            [schedule, credentials, wantedTemp, wantedBright, cameraActivation, aiActive], %data
            ([], (1,512,0)), %reqs(ListOfCharacteristics, (vCPUs, MBmemory,MBstorage))
            [diskController, disk] %linked components
            ).

%%%%%Hardware componenents

componentHW( network1, 
            [networkInput, networkOutput], %data
            [openSystem], %characteristics
            [authentication] %linked components
            ).
componentHW( network2, 
            [networkInput, networkOutput,schedule], %data
            [], %characteristics
            [network2Controller, thermostat, lights, camera, lock] %linked components
            ).
componentHW( thermostat, 
            [thermostatStatus, thermostatTemp, actuationCommands], %data
            [], %characteristics
            [network2] %linked components
            ).
componentHW( lights, 
            [lightStatus, lightBright, actuationCommands], %data
            [], %characteristics
            [network2] %linked components
            ).
componentHW( camera, 
            [cameraActivation, cameraStatus, cameraStream], %data
            [], %characteristics
            [network2] %linked components
            ).
componentHW( lock, 
            [lockStatus, lockCommands], %data
            [], %characteristics
            [network2] %linked components
            ).
componentHW( disk, 
            [storage], %data
            [sharedDisk], %characteristics
            [diskController] %linked components
            ).

componentHW( thermostat2, 
            [thermostatStatus, thermostatTemp, actuationCommands], %data
            [openSystem], %characteristics
            [network2] %linked components
            ).

% lattice of security 
%g_lattice_higherThan(higherElement, lowerElement)
g_lattice_higherThan(top, medium).
g_lattice_higherThan(medium, low).

%policies for data
%tag(variable, security level)
tag(wantedTemp, medium).
tag(wantedBright, medium).
tag(cameraActivation, top).
tag(schedule, top).
tag(aiActive, low).
tag(networkInput, low).
tag(networkOutput, low).
tag(thermostatTemp, low).
tag(lightBright, low).
tag(cameraStream, top).
tag(thermostatStatus, medium).
tag(lightStatus, medium).
tag(cameraStatus, top).
tag(lockStatus, top).
tag(lockCommands, top).
tag(actuationCommands, medium).
tag(cameraLive, top).
tag(credentials, top).
tag(storage, low).

%policies for characteristics
tag(tlsLibrary, top).
tag(openSystem, low).
tag(networkDriver, low).
tag(aiFramework, low).
tag(sharedDisk, low).
tag(diskDriver, low).