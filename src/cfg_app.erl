%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @doc
%%%         cfg application behaviour implementation.
%%% @end
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg_app).
-author('pouriya.jahanbakhsh@gmail.com').
-behaviour(application).
%% -----------------------------------------------------------------------------
%% Exports:

%% 'application' callbacks:
-export([start/2
        ,stop/1]).

%% -----------------------------------------------------------------------------
%% 'application' callbacks:

%% @hidden
start(_, _) -> % (Type, InitArg)
    cfg_sup:start_link().


%% @hidden
stop(_) -> % (State)
    ok.
