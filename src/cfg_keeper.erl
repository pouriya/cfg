-module(cfg_keeper).
-author("pouriya").
-include("cfg_stacktrace.hrl").
%% API
-export([do/2, get/2, get/3, set/3, delete/2, delete/1]).


do({Keeper, Opts}, Cfg) when erlang:is_atom(Keeper) ->
    Mod = module(Keeper),
    try Mod:keep_config(Opts, Cfg) of
        ok ->
            ok;
        {error, {Reason, ErrParams}=Info} when erlang:is_atom(Reason) andalso
                                               erlang:is_map(ErrParams)    ->
            {
                error,
                {
                    keep_config,
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
                    keep_config,
                    #{
                        keeper => Keeper,
                        keeper_options => Opts,
                        returned_value => Other,
                        module => Mod,
                        function => keep_config
                    }
                }
            }
    catch
        ?define_stacktrace(_, Reason, Stacktrace) ->
            {
                error,
                {
                    keep_config,
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
    try Mod:get_config(Opts, Keys) of
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
                        function => get_config,
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
    try Mod:set_config(Opts, Key, Value) of
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
                        function => set_config,
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
    try Mod:delete_config(Opts, Key) of
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
                        function => delete_config,
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
    try Mod:delete_config(Opts) of
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


module(Atom) ->
    erlang:list_to_atom("cfg_keeper_" ++ erlang:atom_to_list(Atom)).
