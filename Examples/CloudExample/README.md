<p><img align="left" width="100"> <h1>Cloud application Example</h1></p>

Here are stored the declaration to run the example of the Cloud application described in the document...
The document describe all the methodology, the formal declarations and the proofs behind *SKnife*.

## Running the example

To run the example the [examples parent directory](https://github.com/di-unipi-socc/sk/tree/main/examples) instruction should be followed.

The eligible partitioning for the application identified by `iotApp1` is given issuing the query `?- sKnife(iotApp1, P).` to the base version of **SKnife**.
The result is:

    P = [((top, safe), [db, authenticator, appManager]),  ((low, safe), [apiGateway]),  ((top, low), [aiLearning]),  ((medium, safe), [userConfig])] ;

Given the non-partitionable application identified by `iotApp2`, the suggestion and the relative eligible partitionings are given issuing the query `?- sKnife(iotApp2, S, P).` to the *Reccomend* version of **SKnife**.
The results are:

    S = [(iotMeasurements, low),  (userPreferences, low)],
    P = [((low, safe), [db, apiGateway, aiLearning2, userConfig2]),  ((top, safe), [authenticator, appManager])] ;

    S = [(aiFramework, top)],
    P = [((top, safe), [db, aiLearning2, authenticator, appManager]),  ((low, safe), [apiGateway]),  ((medium, safe), [userConfig2])] ;

    S = [(dataLibrary, top)],
    P = [((top, safe), [db, authenticator, appManager]),  ((low, safe), [apiGateway]),  ((top, low), [aiLearning2]),  ((medium, safe), [userConfig2])] ;

    S = [(iotMeasurements, medium)],
    P = [((medium, safe), [db, userConfig2]),  ((low, safe), [apiGateway]),  ((medium, low), [aiLearning2]),  ((top, safe), [authenticator, appManager])] ;

    S = [(fromProvider, top)],
    P = [((top, safe), [db, authenticator, appManager]),  ((low, safe), [apiGateway]),  ((top, low), [aiLearning2]),  ((medium, safe), [userConfig2])] ;

    S = [(iotMeasurements, low)],
    P = [((medium, safe), [db, userConfig2]),  ((low, safe), [apiGateway]),  ((medium, low), [aiLearning2]),  ((top, safe), [authenticator, appManager])] ;