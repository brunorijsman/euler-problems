-module(euler_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    euler1:solve(),
    euler2:solve(),
    euler3:solve().

stop(_State) ->
    ok.
