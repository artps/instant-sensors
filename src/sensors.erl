-module(sensors).

-export([start/0]).

ensure_started(App) ->
    case application:start(App) of
        {error, {already_started, App}} -> ok;
        _ -> ok
    end.

start() ->
    sync:go(),

    ensure_started(inets),
    ensure_started(gproc),
    ensure_started(sensors).
