%%%-------------------------------------------------------------------
%%% @author pouriya
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Dec 2019 1:14 PM
%%%-------------------------------------------------------------------
-module(cfg_keeper_ets).
-author("pouriya").

%% API
-export([keep_config/2, get_config/2, set_config/3, delete_config/2, delete_config/1]).


keep_config(Tab, Cfg) when erlang:is_atom(Tab) ->
    InsertFun =
        fun({Key, Value}) ->
            ets:insert(Tab, {Key, Value})
        end,
    lists:foreach(InsertFun, Cfg).


get_config(Tab, Key) when erlang:is_atom(Tab) ->
    case ets:lookup(Tab, Key) of
        [{_, Value}] ->
            {ok, Value};
        _ ->
            not_found
    end.


set_config(Tab, Key, Value) when erlang:is_atom(Tab) ->
    _ = ets:insert(Tab, {Key, Value}),
    ok.


delete_config(Tab, Key) when erlang:is_atom(Tab) ->
    _ = ets:delete(Tab, Key),
    ok.


delete_config(Tab) when erlang:is_atom(Tab) ->
    _ = ets:delete_all_objects(Tab),
    ok.
