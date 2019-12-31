-module(cfg_keeper_env).
-author("pouriya").
%% API
-export([keep_config/2, get_config/2, set_config/3, delete_config/2, delete_config/1]).


keep_config(App, Cfg) when erlang:is_atom(App) ->
    SetEnvFun =
        fun({Key, Value}) ->
            application:set_env(App, Key, Value)
        end,
    lists:foreach(SetEnvFun, Cfg).


get_config(App, Key) when erlang:is_atom(App) ->
    case application:get_env(App, Key) of
        undefined ->
            not_found;
        Ok ->
            Ok
    end.


set_config(App, [Key], Value) when erlang:is_atom(App) ->
    _ = application:set_env(App, Key, Value),
    ok.


delete_config(App, [Key]) when erlang:is_atom(App) ->
    _ = application:unset_env(App, Key),
    ok.


delete_config(App) when erlang:is_atom(App) ->
    UnSetEnvFun =
        fun({Key, _}) ->
            application:unset_env(App, Key)
        end,
    lists:foreach(UnSetEnvFun, application:get_all_env(App)).
