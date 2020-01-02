%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg_reader).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% API
-export([do/1]).

%% -----------------------------------------------------------------------------
%% Behavior callback:

-callback
read_config(Opts :: any()) -> Result when
    Result :: {ok, Config} | {error, {Reason :: atom(), ErrorParams :: map()}},
    Config :: [] | [ConfigParameter],
    ConfigParameter :: atom()
                     | binary()
                     | number()
                     | ConfigList
                     | ConfigProplist,
    ConfigList :: [] | [ConfigParameter],
    ConfigProplist :: [] | [{atom(), ConfigParameter}].

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-include("cfg_stacktrace.hrl").

%% -----------------------------------------------------------------------------
%% API:

do(Readers) ->
    read(Readers, []).

%% -----------------------------------------------------------------------------
%% Internals:

read([{Reader, Param} | Readers], Cfg) when erlang:is_atom(Reader) ->
    case do_read(Reader, Param) of
        {ok, Cfg2} when Cfg == [] ->
            read(Readers, Cfg2);
        {ok, Cfg2} ->
            read(Readers, cfg_utils:override(Cfg, Cfg2));
        Err ->
            Err
    end;

read([], Cfg) ->
    {ok, Cfg};

read([Other|_], _) ->
    {error, {read_config, #{reader_value => Other}}};

read(Other, _) ->
    {error, {read_config, #{readers_value => Other}}}.


do_read(Reader, Opts) ->
    Mod = module(Reader),
    try Mod:read_config(Opts) of
        {ok, Cfg}=Ok ->
            case cfg_utils:is_cfg(Cfg) of
                ok ->
                    Ok;
                {_, Info} ->
                    {
                        error,
                        {
                            read_config,
                            #{
                                reader => Reader,
                                reader_options => Opts,
                                previous_error => Info
                            }
                        }
                    }
            end;
        {error, {Reason, ErrParams}=Info} when erlang:is_atom(Reason) andalso
                                               erlang:is_map(ErrParams)    ->
            {
                error,
                {
                    read_config,
                    #{
                        reader => Reader,
                        reader_options => Opts,
                        previous_error => Info
                    }
                }
            };
        Other ->
            {
                error,
                {
                    read_config,
                    #{
                        reader => Reader,
                        reader_options => Opts,
                        returned_value => Other,
                        module => Mod,
                        function => read_config
                    }
                }
            }
    catch
        ?define_stacktrace(_, Reason, Stacktrace) ->
            {
                error,
                {
                    read_config,
                    #{
                        reader => Reader,
                        reader_options => Opts,
                        exception => Reason,
                        stacktrace => ?get_stacktrace(Stacktrace)
                    }
                }
            }
    end.


module(Atom) ->
    erlang:list_to_atom("cfg_reader_" ++ erlang:atom_to_list(Atom)).
