-module(cfg_filter_test).
-behaviour(cfg_filter).
-export([config_filters/0]).

config_filters() ->
    [{key, atom, default_value}].
