%%%-------------------------------------------------------------------
%%% @author Michal Slaski
%%% @doc 
%%% Starts a process, which inserts 1M elements to a mnesia table.
%%% @end
%%% Created : 25 May 2021 by Michal Slaski
%%%-------------------------------------------------------------------
-module(mnesia_test).

-export([
    start_link/0,
    init/2,
    insert_data/2,
    system_continue/3,
    system_terminate/4,
    system_get_state/1
]).

start_link() ->
    proc_lib:start_link(?MODULE, init, [self(), 1000000]).

init(Parent, N) ->
    register(?MODULE, self()),
    {atomic, ok} = mnesia:create_table(?MODULE, []),
    Keys = lists:seq(1, N),
    proc_lib:init_ack(Parent, {ok, self()}),
    insert_data(Parent, Keys).

insert_data(Parent, [K | Keys]) ->
    ok = mnesia:dirty_write({?MODULE, K, <<K:32>>}),
    insert_data(Parent, Keys);
insert_data(Parent, []) ->
    loop(Parent).

loop(Parent) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], [])
    end.

system_continue(Parent, _Debug, _State) ->
    loop(Parent).

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).

system_get_state(_State) ->
    {ok, mnesia:table_info(?MODULE, all)}.
