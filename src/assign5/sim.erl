-module(sim).
-export([start_sim/1]).

%%  simpler start function to be called externally to start the Watcher-Sensor sim
%%      NumSensors - the total number of sensors that need to be monitored
start_sim(NumSensors) ->
    start_sim_help(NumSensors, 0, 0).

%%  create the correct number of watcher threads based on the total
%%  number of sensors that need to be created and monitored (up to
%%  10 sensors per watcher)
%%      NumSensors - the total number of sensors that need to be monitored
%%      SensorIDCount - used to keep track of where to start the sensorID range for the next watcher
%%      WatcherCount - used to keep track of the number of watcher threads created
start_sim_help(0, _, WatcherCount) ->
    io:fwrite("Successfully created ~p watcher threads~n", [WatcherCount]);

start_sim_help(NumSensors, SensorIDCount, WatcherCount) when NumSensors > 0 ->
    %% get start of SensorID range that the next watcher will watch
    SensorIDStart = SensorIDCount,

    %% get the number of sensors that the next watcher will have to watch
    if
        (NumSensors - 10) > 0 ->
            NumToWatch = 10;
        (NumSensors - 10) < 0 ->
            NumToWatch = (NumSensors rem 10)
    end,

    %% get end of SensorID range that the next watcher will watch
    SensorIDEnd = SensorIDStart + NumToWatch - 1,

    %% spawn next watcher and recurse
    spawn(watcher, watcher_thread, [SensorIDStart, SensorIDEnd]),
    start_sim_help(NumSensors - NumToWatch, SensorIDEnd + 1, WatcherCount + 1).
