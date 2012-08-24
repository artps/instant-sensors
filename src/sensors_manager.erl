-module(sensors_manager).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, SensorsCount} = application:get_env(sensors, count),
    SensorsDataFiles = scan_sensors_data("priv/data", SensorsCount),

    run_sensors(SensorsDataFiles),

    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


run_sensors([Sensor|Sensors]) ->
    SensorId = proplists:get_value(<<"name">>, Sensor),
    Stations = proplists:get_value(<<"data">>, Sensor),

    sensors_sensor_sup:start_child(SensorId, lists:map(fun json_to_term/1, Stations)),

    run_sensors(Sensors);
run_sensors([]) ->
    ok.


scan_sensors_data(Path, Count) ->
    {ok, Files} = file:list_dir(Path),
    read_sensors_data(Files, Count).


read_sensors_data(Files) ->
    read_sensors_data(Files, 10).

read_sensors_data(Files, Max) when is_integer(Max) ->
    read_sensors_data(Files, [], Max).

read_sensors_data(_, Result, 0) ->
    Result;
read_sensors_data([File|Files], Result, Max) ->
    read_sensors_data(Files, Result ++ [read_sensor_data(File)], Max - 1).


json_to_term({Data}) ->
    [
        {lat, proplists:get_value(<<"lat">>, Data)},
        {long, proplists:get_value(<<"long">>, Data)}
    ].

read_sensor_data(Filename) ->
    {ok, Binary} = file:read_file(filename:join(["priv/data", Filename])),

    {Data} = jiffy:decode(Binary),
    Data.


binary_to_float(Binary) ->
    Value = binary_to_list(Binary),
    case string:to_float(Value) of
        {error, no_float} -> list_to_integer(Value);
        {Float, _Rest} -> Float
    end.
