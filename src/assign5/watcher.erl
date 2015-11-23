-module(watcher).
-export([watcher_thread/2]).

%%  Called externally from sim to initialize watcher thread.
%%      SensorIDStart - starting ID in the sensor ID range that this watcher must keep track of
%%      SensorIDEnd - ending ID in the sensor ID range that this watcher must keep track of
watcher_thread(SensorIDStart, SensorIDEnd) ->
    Sensors = create_sensors(SensorIDStart, SensorIDEnd, []),
    print_sensors(Sensors),
    watch_loop(Sensors).

%%  Create sensors with ID's ranging from CurrSensorID to SensorIDEnd.
%%      CurrSensorID - used to keep track of the sensor ID of the next sensor to be created (initial call should initialize it to SensorIDStart)
%%      SensorIDEnd - ending ID in the sensor ID range that this watcher must keep track of
%%      SensorList - temporary compiled list of {SensorID, Pid} tuples
create_sensors(CurrSensorID, SensorIDEnd, SensorList) when CurrSensorID > SensorIDEnd ->
    %% base case: return compiled SensorList
    SensorList;

create_sensors(CurrSensorID, SensorIDEnd, SensorList) when CurrSensorID =< SensorIDEnd ->
    {Pid, _} = spawn_monitor(sensor, sensor_thread, [self(), CurrSensorID]),
    create_sensors(CurrSensorID + 1, SensorIDEnd, SensorList ++ [{CurrSensorID, Pid}]).

%%  Prints out a {SensorID, Pid} tuple for every sensor that this watcher is keeping track of.
print_sensors(Sensors) ->
    io:fwrite("Watcher ~p watching: ~p~n~n", [self(), Sensors]).

%%  Loop to recieve reports from watched sensors.
%%      Sensors - list containing the {SensorID, Pid} tuples for all of the sensors that this watcher is keeping track of
watch_loop(Sensors) ->
    receive
        {SensorID, Report} ->
            if
                Report == "anomalous_reading" ->
                    %% if a crash was reported print the crash report, spawn a new sensor thread for that SensorID,
                    %% print the updated Sensors list, and recurse on watch_loop with the new list
                    io:fwrite("Watcher ~p: Sensor ~p has crashed (ERROR: ~p)~n", [self(), SensorID, "anomalous_reading"]),

                    {Pid, _} = spawn_monitor(sensor, sensor_thread, [self(), SensorID]),

                    %% filter the Sensors list to find the element to be deleted
                    Elem = lists:last(lists:filter(fun({StoredSid, _}) ->
                                                        StoredSid == SensorID
                                                   end, Sensors)),

                    NewSensorsList = lists:append( lists:delete(Elem, Sensors), [{SensorID, Pid}] ),
                    print_sensors(NewSensorsList);
                true ->
                    %% print measurement report if no crash was reported
                    io:fwrite("Watcher ~p: Sensor ~p reported measurement ~p~n", [self(), SensorID, Report]),
                    NewSensorsList = Sensors
            end
    end,
    watch_loop(NewSensorsList).
