%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg_utils).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% API:
-export([is_proplist/1]).

%% -----------------------------------------------------------------------------
%% API:

is_proplist([{Key, _}|Rest]) when erlang:is_atom(Key) ->
    is_proplist(Rest);

is_proplist([]) ->
    true;

is_proplist(_) ->
    false.

