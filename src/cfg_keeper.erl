%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg_keeper).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% API
-export(
    [
        init/1,
        load/2,
        get/2,
        get/3,
        set/3,
        delete/2,
        delete/1
    ]
).

%% -----------------------------------------------------------------------------
%% Behavior callbacks:

%% Initializing keeoer:
-callback
config_init(Opts :: any()) ->
    ok | {error, {Reason :: atom(), ErrorParams :: map()}}.

%% Accepts all configuration parameters as a proplist and loads them in keeper:
-callback
config_load(Opts :: any(), [] | [{atom(), term()}]) ->
    ok | {error, {Reason :: atom(), ErrorParams :: map()}}.

%% Tries to get key's corresponding value:
-callback
config_get(Opts :: any(), Key :: atom()) ->
    {ok, Value :: term()}                             |
    {error, {Reason :: atom(), ErrorParams :: map()}} |
    not_foud.

%% Sets key's corresponding value to 'Value' in keeper:
-callback
config_set(Opts :: any(), Key :: atom(), Value :: term()) ->
    ok | {error, {Reason :: atom(), ErrorParams :: map()}}.

%% Deletes key from keeper:
-callback
config_delete(Opts :: any(), Key :: atom()) ->
    ok | {error, {Reason :: atom(), ErrorParams :: map()}}.

%% Deletes all configuration and cleans everything related to keeper:
-callback
config_delete(Opts :: any()) ->
    ok | {error, {Reason :: atom(), ErrorParams :: map()}}.

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-include("cfg_stacktrace.hrl").

%% -----------------------------------------------------------------------------
%% API:

init({Keeper, Opts}) ->
    Mod = module(Keeper),
    try Mod:config_init(Opts) of
        ok ->
            ok;
        {error, {Reason, ErrParams}=Info} when erlang:is_atom(Reason) andalso
                                               erlang:is_map(ErrParams)    ->
            {
                error,
                {
                    load_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        previous_error => Info
                    }
                }
            };
        Other ->
            {
                error,
                {
                    load_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        returned_value => Other,
                        module => Mod,
                        function => config_init
                    }
                }
            }
    catch
        ?define_stacktrace(_, Reason, Stacktrace) ->
            {
                error,
                {
                    load_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        exception => Reason,
                        stacktrace => ?get_stacktrace(Stacktrace)
                    }
                }
            }
    end.


load({Keeper, Opts}, Cfg) when erlang:is_atom(Keeper) ->
    Mod = module(Keeper),
    try Mod:config_load(Opts, Cfg) of
        ok ->
            ok;
        {error, {Reason, ErrParams}=Info} when erlang:is_atom(Reason) andalso
                                               erlang:is_map(ErrParams)    ->
            {
                error,
                {
                    load_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        previous_error => Info
                    }
                }
            };
        Other ->
            {
                error,
                {
                    load_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        returned_value => Other,
                        module => Mod,
                        function => config_load
                    }
                }
            }
    catch
        ?define_stacktrace(_, Reason, Stacktrace) ->
            {
                error,
                {
                    load_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        exception => Reason,
                        stacktrace => ?get_stacktrace(Stacktrace)
                    }
                }
            }
    end.


get({Keeper, Opts}, Keys) when erlang:is_atom(Keeper) ->
    Mod = module(Keeper),
    try Mod:config_get(Opts, Keys) of
        {ok, _}=Ok ->
            Ok;
        not_found ->
            not_found;
        {error, {Reason, ErrParams}=Info} when erlang:is_atom(Reason) andalso
                                               erlang:is_map(ErrParams)    ->
            {
                error,
                {
                    get_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        keys => Keys,
                        previous_error => Info
                    }
                }
            };
        Other ->
            {
                error,
                {
                    get_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        returned_value => Other,
                        module => Mod,
                        function => config_get,
                        keys => Keys
                    }
                }
            }
    catch
        ?define_stacktrace(_, Reason, Stacktrace) ->
            {
                error,
                {
                    get_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        keys => Keys,
                        exception => Reason,
                        stacktrace => ?get_stacktrace(Stacktrace)
                    }
                }
            }
    end.


get({Keeper, _}=KeeperOpts, Keys, Default) when erlang:is_atom(Keeper) ->
    case get(KeeperOpts, Keys) of
        not_found ->
            {ok, Default};
        Other ->
            Other
    end.


set({Keeper, Opts}, Key, Value) when erlang:is_atom(Keeper) ->
    Mod = module(Keeper),
    try Mod:config_set(Opts, Key, Value) of
        ok ->
            ok;
        {error, {Reason, ErrParams}=Info} when erlang:is_atom(Reason) andalso
                                               erlang:is_map(ErrParams)    ->
            {
                error,
                {
                    set_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        key => Key,
                        value => Value,
                        previous_error => Info
                    }
                }
            };
        Other ->
            {
                error,
                {
                    set_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        returned_value => Other,
                        module => Mod,
                        function => config_set,
                        key => Key,
                        value => Value
                    }
                }
            }
    catch
        ?define_stacktrace(_, Reason, Stacktrace) ->
            {
                error,
                {
                    set_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        key => Key,
                        value => Value,
                        exception => Reason,
                        stacktrace => ?get_stacktrace(Stacktrace)
                    }
                }
            }
    end.


delete({Keeper, Opts}, Key) when erlang:is_atom(Keeper) ->
    Mod = module(Keeper),
    try Mod:config_delete(Opts, Key) of
        ok ->
            ok;
        not_found ->
            not_found;
        {error, {Reason, ErrParams}=Info} when erlang:is_atom(Reason) andalso
                                               erlang:is_map(ErrParams)    ->
            {
                error,
                {
                    delete_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        key => Key,
                        previous_error => Info
                    }
                }
            };
        Other ->
            {
                error,
                {
                    delete_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        returned_value => Other,
                        module => Mod,
                        function => config_delete,
                        key => Key
                    }
                }
            }
    catch
        ?define_stacktrace(_, Reason, Stacktrace) ->
            {
                error,
                {
                    delete_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        key => Key,
                        exception => Reason,
                        stacktrace => ?get_stacktrace(Stacktrace)
                    }
                }
            }
    end.


delete({Keeper, Opts}) when erlang:is_atom(Keeper) ->
    Mod = module(Keeper),
    try Mod:config_delete(Opts) of
        ok ->
            ok;
        {error, {Reason, ErrParams}=Info} when erlang:is_atom(Reason) andalso
                                               erlang:is_map(ErrParams)    ->
            {
                error,
                {
                    delete_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        previous_error => Info
                    }
                }
            };
        Other ->
            {
                error,
                {
                    delete_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        returned_value => Other,
                        module => Mod,
                        function => delete_config
                    }
                }
            }
    catch
        ?define_stacktrace(_, Reason, Stacktrace) ->
            {
                error,
                {
                    delete_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        exception => Reason,
                        stacktrace => ?get_stacktrace(Stacktrace)
                    }
                }
            }
    end.

%% -----------------------------------------------------------------------------
%% Internals:

module(env) ->
    cfg_keeper_env;

module(ets) ->
    cfg_keeper_ets;

module(Atom) ->
    erlang:list_to_atom("cfg_keeper_" ++ erlang:atom_to_list(Atom)).
