-module(sensors_sensor).

-behaviour(gen_fsm).

-export([start_link/1]).
-export([stop/1]).

-export([init/1,
		 state_idle/2, state_idle/3,
		 state_working/2, state_working/3,
		 handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-define(SERVER, ?MODULE).
-define(IDLE_TIMEOUT, 15000).

-record(state, { sensor_id, lat, long, timestamp }).


start_link(SensorId) ->
	gen_fsm:start_link(?MODULE, [SensorId], []).


init([SensorId]) ->
    {ok, state_idle, #state{
        sensor_id = SensorId,
        timestamp = get_timestamp(),
        lat = get_lat(),
        long = get_long()
    }, ?IDLE_TIMEOUT}.


state_idle(timeout, State) ->
    gen_fsm:send_event(self(), send_data),
    {next_state, state_working, State};

state_idle(Msg, State) ->
	{next_state, state_idle, ?IDLE_TIMEOUT}.


state_working(send_data, State) ->
    NewState = State#state{
        timestamp = get_timestamp(),
        lat = get_lat(State),
        long = get_long(State)
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
	Reply = ok,
	{reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
	{next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.


get_timestamp() ->
    timer:now_diff(now(), {0,0,0}) div 1000000.

get_lat() ->
    55.794262.
get_lat(State) ->
    State#state.lat + random:uniform().

get_long() ->
    49.114468.
get_long(State) ->
    State#state.long + random:uniform().


make_sensor_url(Collector, SensorId) when is_integer(SensorId) ->
    make_sensor_url(Collector, integer_to_list(SensorId));
make_sensor_url(Collector, SensorId) ->

    Host = proplists:get_value(host, Collector),
    Port = proplists:get_value(port, Collector),
    Protocol = proplists:get_value(protocol, Collector),

    Protocol ++ "://" ++ filename:join([Host ++ ":" ++ Port, "sensors", SensorId]).

send_data(State) ->
    {ok, Collector} = application:get_env(sensors, collector),
    Body = jiffy:encode({[
        {timestamp, State#state.timestamp},
        {lat, State#state.lat},
        {long, State#state.long}
    ]}),
    Url = make_sensor_url(Collector, State#state.sensor_id),
    case httpc:request(put, {Url, [], "application/json", Body}, [], []) of
        {ok, Response} ->
            Response;
        {error, Error} ->
            {error, [], []}
    end.
