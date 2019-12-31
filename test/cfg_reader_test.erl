%%%-------------------------------------------------------------------
%%% @author pouriya
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Dec 2019 2:01 PM
%%%-------------------------------------------------------------------
-module(cfg_reader_test).
-author("pouriya").

%% API
-export([read_config/1]).


read_config({exception, Reason}) ->
    erlang:error(Reason);

read_config(X) ->
    X.