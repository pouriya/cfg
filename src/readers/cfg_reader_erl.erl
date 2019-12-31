%%%-------------------------------------------------------------------
%%% @author pouriya
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Dec 2019 1:38 PM
%%%-------------------------------------------------------------------
-module(cfg_reader_erl).
-author("pouriya").

%% API
-export([read_config/1]).


read_config(Filename) ->
    case file:consult(Filename) of
        {error, Reason} ->
            {
                error,
                {
                    erl,
                    #{
                        filename => Filename,
                        reason => Reason,
                        info => file:format_error(Reason)
                    }
                }
            };
        Ok ->
            Ok
    end.