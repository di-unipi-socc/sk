

<p><img align="left" width="100"> <h1>SKnife</h1></p>
SKnife is a declarative prototype to partition multi-component applications employing information-flow security methodologies in order to exploit the Separation Kernel (SK) technology.
SKnife first check the application is partitionable, i.e. do not leak data to untrusted hardware components, then finds the minimal eligible partitioning, the partitioning with the fewer number of SK domain that avoids data leak.
To support the developers of non-partitionable applications, SKnife-Recommend allows finding labelling suggestions to relax the information-flow constraints in order to allow the partitioning.

<br></br>
## Prerequisites

Before using **SKnife** you need to install the latest stable release of [SWI-Prolog]
## Tutorial

To try **SKnife** *Base* version:

1. Download or clone this repository.

2. Open a terminal in the project folder and run `swipl skplacer.pl`.

3. Inside the running program either run the query
   ```prolog
   :- sKnife(AppIdentifier, Partitioning).
   ``` 
   The output is the elegible partitioning for the application described in `model.pl` .
    A Partitioning is composed by the list of domains that compose the partitioning. A domain is composed by the pair of labelling representing the *trust* and the *secrecy* of the hosted components, the list of the hosted components and the hardware requirements need by the software components.
   E.g. of eligible placment
   ```prolog
   Partitioning = 
   [((top, safe), [diskController, lockController, cameraController, Network2Controller, authentication, businessLogic, userConfig], 2, 3328, 640), 
    ((medium, safe), [lightsController, thermostatController], 1, 1024, 0),
    ((low, safe), [network1Controller], 1, 512, 0),  ((top, low), [aiLearning], 4, 2048, 0)]
   ```
   
To try **SKnife** *Recommend* version:
1. Download or clone this repository.

2. Open a terminal in the project folder and run ``swipl skplacerReccomend.pl``.

3. Inside the running program either run the query
   ```prolog
   :- sKnife(iotApp2, Suggestion, Partitioning).
   ```  

   The outputs are the suggestions and the relative eligible partitioning for the non-partitionable application ``iotApp2``
   E.g. of suggestion
    ```prolog
   Suggestion = [(thermostatStatus, low),  (actuationCommands, low)],
   Partitioning = [((top, safe), [diskController, lockController, cameraController, network2Controller, authentication, businessLogic, userConfig], 2, 3328, 640),  ((medium, afe), [lightsController, thermostatController], 1, 1024, 0),  ((low, safe), [network1Controller], 1, 512, 0),  ((top, low), [aiLearning], 4, 2048, 0)]1
   ```
  This indicate that changing the labelling of ``thermostatStatus`` and ``actuationCommands`` to the label ``low`` is possible to partition the application and the eligibile partitioning is given by ``Partitioning``.
