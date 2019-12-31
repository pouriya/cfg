-module(cfg_test_utils).

%% API
-export([clean_env/0]).


clean_env() ->
    lists:foreach(
        fun({K, _}) ->
            application:unset_env(cfg, K)
        end,
        application:get_all_env(cfg)
    ).