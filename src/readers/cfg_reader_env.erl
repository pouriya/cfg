%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg_reader_env).
-behaviour(cfg_reader).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% 'config_reader' callback:
-export([read_config/1]).

%% -----------------------------------------------------------------------------
%% 'config_reader' callback:

read_config(App) when erlang:is_atom(App) ->
    {ok, application:get_all_env(App)};

read_config({App, Key}) when erlang:is_atom(App) andalso erlang:is_atom(Key) ->
    case application:get_env(App, Key) of
        {ok, _}=Ok ->
            Ok;
        _ -> % undefined
            {
                error,
                {
                    env,
                    #{
                        reason => key_not_found,
                        key => Key,
                        application => App
                    }
                }
            }
    end.
