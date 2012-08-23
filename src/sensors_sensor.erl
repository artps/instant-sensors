-module(sensors_sensor).

-behaviour(gen_fsm).

-export([start_link/2]).
-export([stop/1]).

-export([init/1,
        state_idle/2, state_idle/3,
        state_working/2, state_working/3,
        handle_event/3, handle_sync_event/4, handle_info/3,
        terminate/3, code_change/4]).

-define(SERVER, ?MODULE).
-define(IDLE_TIMEOUT, 15000).

-record(state, { sensor_id, timestamp, station, stations }).


start_link(SensorId, Stations) ->
    gen_fsm:start_link(?MODULE, [SensorId, Stations], []).


init([SensorId, Stations]) ->
    true = gproc:add_local_name(SensorId),
    {ok, state_idle, #state{
        sensor_id = SensorId,
        timestamp = get_timestamp(),
        station = 0,
        stations = Stations
    }, ?IDLE_TIMEOUT}.


state_idle(timeout, State) ->
    gen_fsm:send_event(self(), send_data),
    {next_state, state_working, State};

state_idle(Msg, State) ->
    {next_state, state_idle, ?IDLE_TIMEOUT}.


state_working(send_data, State) ->
    NewState = State#state{
        timestamp = get_timestamp(),
        station = State#state.station + 1
    },

    {Status, Headers, Body} = send_data(NewState),

    error_logger:info_msg("ID: ~p, Response: ~p~n", [
        State#state.sensor_id, Status
    ]),

    {next_state, state_idle, NewState, ?IDLE_TIMEOUT div 2}.

state_idle(_Event, _From, State) ->
    {reply, ok, state_idle, State, ?IDLE_TIMEOUT}.


state_working(_Event, _From, State) ->
    {reply, ok, state_idle, State, ?IDLE_TIMEOUT}.


handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData};

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).


handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


get_timestamp() ->
    timer:now_diff(now(), {0,0,0}) div 1000000.


make_sensor_url(Collector, SensorId) when is_integer(SensorId) ->
    make_sensor_url(Collector, integer_to_list(SensorId));
make_sensor_url(Collector, SensorId) ->
    Host = proplists:get_value(host, Collector),
    Port = integer_to_list(proplists:get_value(port, Collector)),
    Protocol = proplists:get_value(protocol, Collector),

    lists:flatten([Protocol, "://", Host, ":", Port, "/sensors/", SensorId]).

get_coordinates(State) ->
    lists:nth(State#state.station, State#state.stations).

send_data(State) ->
    {ok, Collector} = application:get_env(sensors, collector),
    Coordinates = get_coordinates(State),
    Body = jiffy:encode({[
        {timestamp, State#state.timestamp},
        {lat, proplists:get_value(lat, Coordinates)},
        {long, proplists:get_value(long, Coordinates)}
    ]}),
    Url = make_sensor_url(Collector, State#state.sensor_id),
    case httpc:request(put, {Url, [], "application/json", Body}, [], []) of
        {ok, Response} ->
            Response;
        {error, Error} ->
            {error, [], []}
    end.
