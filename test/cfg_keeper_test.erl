%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg_keeper_test).
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

config_init({exception, Reason}) ->
    erlang:error(Reason);

config_init(X) ->
    X.


config_set(X, Y) ->
    X(Y).


config_get({exception, Reason}) ->
    erlang:error(Reason);

config_get(X) ->
    X.


config_get(X, Y) ->
    X(Y).


config_set(X, Y, Z) ->
    X(Y, Z).


config_delete(X, Y) ->
    X(Y).


config_delete({exception, Reason}) ->
    erlang:error(Reason);

config_delete(X) ->
    X.
