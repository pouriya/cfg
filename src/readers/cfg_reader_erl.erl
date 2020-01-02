%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg_reader_erl).
-behaviour(cfg_reader).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% 'config_reader' callback:
-export([read_config/1]).

%% -----------------------------------------------------------------------------
%% 'config_reader' callback:

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
