-module(sensors).

-export([start/0]).

start() ->
    application:start(inets),
    application:start(gproc),
    application:start(sensors),

    sync:go().
