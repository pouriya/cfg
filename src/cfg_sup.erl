%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @doc
%%%         cfg root supervisor implementation.
%%% @end
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg_sup).
-author('pouriya.jahanbakhsh@gmail.com').
-behaviour(supervisor).
%% -----------------------------------------------------------------------------
%% Exports:

%% 'application' API:
-export([start_link/0]).

%% 'supervisor' callback:
-export([init/1]).

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(PROC, ?MODULE).

%% -----------------------------------------------------------------------------
%% API:

-spec
start_link() ->
    {ok, pid()} | {error, term()} | ignore.
start_link() ->
    supervisor:start_link({local, ?PROC}, ?MODULE, undefined).


%% -----------------------------------------------------------------------------
%% 'supervisor' callbacks:

%% @hidden
init(_) -> % (undefined)
    {ok, { {one_for_all, 0, 1}, []} }.
