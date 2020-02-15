%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg_keeper_env).
-behaviour(cfg_keeper).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% 'cfg_keeper' callbacks:
-export(
    [
        config_init/1,
        config_set/2,
        config_set/3,
        config_get/1,
        config_get/2,
        config_delete/2,
        config_delete/1
    ]
).

%% -----------------------------------------------------------------------------
%% 'cfg_keeper' callbacks:

config_init(App) when erlang:is_atom(App) ->
    ok;

config_init({App, EnvKey}) when erlang:is_atom(App) andalso
                                erlang:is_atom(EnvKey)   ->
    ok.


config_set(App, Cfg) when erlang:is_atom(App) ->
    SetEnvFun =
        fun({Key, Value}) ->
            application:set_env(App, Key, Value)
        end,
    lists:foreach(SetEnvFun, Cfg);

config_set({App, EnvKey}, Cfg) when erlang:is_atom(App) andalso
                                    erlang:is_atom(EnvKey)   ->
    _ = application:set_env(App, EnvKey, Cfg),
    ok.


config_get(App) when erlang:is_atom(App) ->
    {ok, application:get_all_env(App)};

config_get({App, EnvKey}) when erlang:is_atom(App) ->
    case application:get_env(App, EnvKey) of
        {_, Cfg} ->
            {ok, Cfg};
        _ ->
            {ok, []}
    end.


config_get(App, Key) when erlang:is_atom(App) ->
    case application:get_env(App, Key) of
        undefined ->
            not_found;
        Ok ->
            Ok
    end;

config_get({App, EnvKey}, Key) when erlang:is_atom(App) andalso
                                    erlang:is_atom(EnvKey)   ->
    case application:get_env(App, EnvKey) of
        {_, Cfg} ->
            case lists:keyfind(Key, 1, Cfg) of
                {_, Value} ->
                    {ok, Value};
                _ ->
                    not_found
            end;
        _ -> % undefined
            not_found
    end.


config_set(App, Key, Value) when erlang:is_atom(App) ->
    _ = application:set_env(App, Key, Value),
    ok;

config_set({App, EnvKey}, Key, Value) when erlang:is_atom(App) andalso
                                           erlang:is_atom(EnvKey)   ->
    case application:get_env(App, EnvKey) of
        {_, Cfg} ->
            _ = application:set_env(
                App,
                EnvKey,
                lists:keyreplace(Key, 1, Cfg, {Key, Value})
            ),
            ok;
        _ -> % undefined
            _ = application:set_env(App, EnvKey, [{Key, Value}]),
            ok
    end.


config_delete(App, Key) when erlang:is_atom(App) ->
    _ = application:unset_env(App, Key),
    ok;

config_delete({App, EnvKey}, Key) when erlang:is_atom(App) andalso
                                       erlang:is_atom(EnvKey)   ->
    case application:get_env(App, EnvKey) of
        {_, Cfg} ->
            _ = application:set_env(App, EnvKey, lists:keydelete(Key, 1, Cfg)),
            ok;
        _ -> % undefined
            ok
    end.


config_delete(App) when erlang:is_atom(App) ->
    UnSetEnvFun =
        fun({Key, _}) ->
            application:unset_env(App, Key)
        end,
    lists:foreach(UnSetEnvFun, application:get_all_env(App)); % ok

config_delete({App, EnvKey}) when erlang:is_atom(App) andalso
                                  erlang:is_atom(EnvKey)   ->
    _ = application:unset_env(App, EnvKey),
    ok.
