-module(cfg_test_utils).

%% API
-export([clean_env/0, set_env/1, config/1]).


clean_env() ->
    lists:foreach(
        fun({K, _}) ->
            application:unset_env(cfg, K)
        end,
        application:get_all_env(cfg)
    ).

set_env(List) ->
    lists:foreach(fun({K, V}) -> application:set_env(cfg, K, V) end, List).


config(Cfg) ->
    _ = cfg_test_utils:clean_env(),
    _ = cfg_test_utils:set_env(Cfg),
    {ok, _, Cfg2} = cfg:read([{env, cfg}]),
    Cfg2.