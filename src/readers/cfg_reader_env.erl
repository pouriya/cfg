-module(cfg_reader_env).
-author("pouriya").

%% API
-export([read_config/1]).


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
    end;

read_config(Other) ->
    {error, {env, #{app_name => Other}}}.
