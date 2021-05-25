%%%-------------------------------------------------------------------
%% @doc elab_demo public API
%% @end
%%%-------------------------------------------------------------------

-module(elab_demo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    elab_demo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
