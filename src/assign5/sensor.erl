-module(sensor).
-export([sensor_thread/2]).

%%  Called externally from watcher to initialize sensor thread.
%%      WatcherPid - the pid of the watcher that is watching this sensor
%%      SensorID - the unique ID assigned to this sensor
sensor_thread(WatcherPid, SensorID) ->
    %% get random measurement between 1-11
    random:seed(erlang:now()),
    Measurement = random:uniform(11),

    %% if Measurement is 11, report anomalous_reading and crash
    %% else report Measurement
    if
        Measurement == 11 ->
            WatcherPid ! {self(), SensorID, "anomalous_reading"},
            exit(crash);
        true ->
            WatcherPid ! {self(), SensorID, Measurement}
    end,

    %% sleep for random amount of time and then recurse
    SleepTime = random:uniform(10000),
    timer:sleep(SleepTime),
    sensor_thread(WatcherPid, SensorID).
