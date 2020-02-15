%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg_reader).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% API
-export(
    [
        do/1,
        is_cfg/1
    ]
).

%% -----------------------------------------------------------------------------
%% Behavior callback:

-callback
read_config(Opt :: any()) -> Result when
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

do(Readers) when erlang:is_list(Readers) andalso Readers /= [] ->
    read(Readers, [], []).

%% -----------------------------------------------------------------------------
%% Internals:

read(
    [{Backend, Opt}=Reader | Readers],
    SeparateCfg,
    FinalCfg
) when erlang:is_atom(Backend) ->
    case do_read(Backend, Opt) of
        {ok, Cfg} ->
            read(
                Readers,
                SeparateCfg ++ [{Backend, Opt, Cfg}],
                override(add_reader_info(Cfg, Reader), FinalCfg, [])
            );
        Err ->
            Err
    end;

read([], SeparateCfg, Cfg) ->
    {ok, SeparateCfg, Cfg};

read([Other|_], _, _) ->
    {error, {read_config, #{reason => bad_reader, reader => Other}}}.


do_read(Reader, Opt) ->
    Mod = module(Reader),
    try Mod:read_config(Opt) of
        {ok, Cfg}=Ok ->
            case is_cfg(normalize(Cfg)) of
                ok ->
                    Ok;
                {_, Info} ->
                    {
                        error,
                        {
                            read_config,
                            #{
                                reader => Reader,
                                reader_option => Opt,
                                previous_error => Info
                            }
                        }
                    }
            end;
        {error, ErrParams} when erlang:is_map(ErrParams) ->
            {
                error,
                {
                    read_config,
                    ErrParams#{
                        reader => Reader,
                        reader_option => Opt
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
                        reader_option => Opt,
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
                        reader_option => Opt,
                        exception => Reason,
                        stacktrace => ?get_stacktrace(Stacktrace)
                    }
                }
            }
    end.


module(env) ->
    cfg_reader_env;

module(erl) ->
    cfg_reader_erl;

module(shell) ->
    cfg_reader_shell;

module(Atom) ->
    erlang:list_to_atom("cfg_reader_" ++ erlang:atom_to_list(Atom)).


%% The purpose is to convert strings to binaries and maps to proplists:
normalize([{X, Y} | Cfg]) ->
    [{X, normalize(Y)} | normalize(Cfg)];

normalize(Map) when erlang:is_map(Map) ->
    normalize(maps:to_list(Map));

normalize(List) when erlang:length(List) > 0 ->
    case io_lib:printable_unicode_list(List) of
        true ->
            erlang:list_to_binary(List);
        _ ->
            [normalize(X) || X <- List]
    end;

normalize(X) ->
    X.


is_cfg(Cfg) ->
    % It must be a proplist in first level:
    case cfg_utils:is_proplist(Cfg) of
        true ->
            case check_value(Cfg) of
                {_, {Reason, #{parent_keys := ParentKeys}=ErrParams}} ->
                    [Key | ParentKeys2] = lists:reverse(ParentKeys),
                    {
                        error,
                        {
                            Reason,
                            ErrParams#{
                                parent_keys => ParentKeys2,
                                key => Key
                            }
                        }
                    };
                Other ->
                    Other
            end;
        _ ->
            {error, {check_config, #{allowed_type => proplist}}}
    end.

check_value(List) when erlang:is_list(List) ->
    check_list(List);

check_value(Value) when erlang:is_number(Value) orelse
                        erlang:is_atom(Value)   orelse
                        erlang:is_binary(Value)     ->
    ok;

check_value(Other) ->
    {
        error,
        {
            check_config,
            #{
                value => Other,
                allowed_types => [proplist, list, atom, binary, number]
            }
        }
    }.


check_list([{Key, Value} | List]) ->
    if
        erlang:is_atom(Key) ->
            case check_value(Value) of
                ok ->
                    check_list(List);
                {_, {Reason, #{parent_keys := ParentKeys}=ErrParam}} ->
                    {
                        error,
                        {Reason, ErrParam#{parent_keys => [Key | ParentKeys]}}
                    };
                {_, {Reason, ErrParam}} ->
                    {error, {Reason, ErrParam#{parent_keys => [Key]}}}
            end;
        true ->
            {
                error,
                {
                    check_config,
                    #{
                        reason => bad_key,
                        parent_keys => [Key],
                        allowed_type => atom
                    }
                }
            }
    end;

check_list([Item | List]) ->
    case check_value(Item) of
        ok ->
            check_list(List);
        _ ->
            {
                error,
                {
                    check_config,
                    #{
                        list_element => Item,
                        allowed_types => [proplist, list, atom, binary, number]
                    }
                }
            }
    end;

check_list([]) ->
    ok;

check_list(Other) ->
    {
        error,
        {
            check_config,
            #{
                list => Other,
                allowed_type => list
            }
        }
    }.


add_reader_info([{Key, Value}|Cfg], Reader) ->
    [
        {Key, add_reader_info(Value, Reader), [Reader]} |
        add_reader_info(Cfg, Reader)
    ];

add_reader_info([Value | Cfg], Reader) ->
    [Value|add_reader_info(Cfg, Reader)];

add_reader_info(Value, _) ->
    Value.


override([{Key, Value1, Reader1}=Item|Cfg1], Cfg2, Ret) ->
    case lists:keytake(Key, 1, Cfg2) of
        {_, {_, Value2, Reader2}, Cfg3} ->
            {Reader3, Value3} =
                case
                    Value1 /= Value2    andalso
                    is_proplist(Value1) andalso
                    is_proplist(Value2)
                of
                    true ->
                        {Reader1 ++ Reader2, override(Value1, Value2, [])};
                    _ ->
                        {Reader1, Value1}
                end,
            override(Cfg1, Cfg3, [{Key, Value3, Reader3} | Ret]);
        _ ->
            override(Cfg1, Cfg2, [Item | Ret])
    end;

override(_, Cfg2, Ret) ->
    lists:reverse(Ret) ++ Cfg2.


is_proplist([{Key, _, _}|Rest]) when erlang:is_atom(Key) ->
    is_proplist(Rest);

is_proplist([]) ->
    true;

is_proplist(_) ->
    false.
