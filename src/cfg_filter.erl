%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg_filter).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% API
-export(
    [
        do/2,
        check/1,
        get_filters/1
    ]
).

%% -----------------------------------------------------------------------------
%% Behaviour callback:

-callback
config_filters() -> Filters when
    Filters :: [] | [Filter],
    Filter :: any | '_'
            | atom
            | binary
            | number
            | integer
            | float
            | list
            | boolean
            | proplist
            | atom_to_list
            | list_to_atom
            | list_to_binary
            | list_to_integer
            | list_to_float
            | binary_to_list
            | binary_to_integer
            | binary_to_float
            | atom_to_binary
            | binary_to_atom
            | proplist_to_map
            | {'&', Filters} | {'and', Filters}
            | {'|', Filters} | {'or', Filters}
            | {proplist, Filters}
            | {list, Filters}
            | {f, function()}
            | {mf, {module(), FunctionName :: atom()}}
            | {one_of, [any()]}
            | {size, Size},
    Size :: number()
          | {min, number()}
          | {max, number()}
          | {Min :: number(), Max :: number()}.

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-include("cfg_stacktrace.hrl").

%% -----------------------------------------------------------------------------
%% API:

do(AppName, Cfg) when erlang:is_atom(AppName) ->
    case get_filters(AppName) of
        {ok, Filters} ->
            do(Filters, Cfg);
        Err ->
            Err
    end;

do(Filters, Cfg) ->
    case filter(Filters, Cfg, [], [], 1) of
        {_, ErrParams} ->
            {error, {filter_config, ErrParams#{filters => Filters}}};
        Ok ->
            Ok
    end.


check(Filters) ->
    case check_filters(Filters, 1) of
        ok ->
            ok;
        {_, ErrParams} ->
            {error, {check_filters, ErrParams#{filters => Filters}}}
    end.


get_filters(AppName) ->
    case application:get_key(AppName, modules) of
        {ok, Mods} ->
            case get_modules_filters(Mods, []) of
                {ok, _}=Ok ->
                    Ok;
                {_, {Reason, ErrParams}} ->
                    {error, {Reason, ErrParams#{application => AppName}}}
            end;
        _ -> % undefined
            {
                error,
                {
                    application_filters,
                    #{application => AppName, reason => not_found}
                }
            }
    end.

%% -----------------------------------------------------------------------------
%% Internals:

filter(
    [{Key, KeyFilter, Default}=Filter | Filters],
    Cfg,
    Unknown,
    Ret,
    Index
) ->
    case filter_key(Key, KeyFilter, Cfg) of
        {ok, Key2, Value, Unknown2, Cfg2} ->
            filter(
                Filters,
                Cfg2,
                maybe_add_unknown_config(Key2, Unknown2, Unknown),
                [{Key2, Value} | Ret],
                Index + 1
            );
        {_, #{key := Key, reason := not_found}} ->
            filter(Filters, Cfg, Unknown, [{Key, Default} | Ret], Index + 1);
        {Reason, ErrParams} ->
            {Reason, ErrParams#{filter => Filter, filter_index => Index}}
    end;

filter([{Key, KeyFilter}=Filter | Filters], Cfg, Unknown, Ret, Index) ->
    case filter_key(Key, KeyFilter, Cfg) of
        {ok, Key2, Value, Unknown2, Cfg2} ->
            filter(
                Filters,
                Cfg2,
                maybe_add_unknown_config(Key2, Unknown2, Unknown),
                [{Key2, Value} | Ret],
                Index + 1
            );
        {_, ErrParams} ->
            {error, ErrParams#{filter => Filter}}
    end;

filter([], Cfg, Unknown, Ret, _) ->
    {ok, lists:reverse(Ret), Unknown ++ Cfg}.


maybe_add_unknown_config(_, [], X) ->
    X;

maybe_add_unknown_config(Key, Value, X) ->
    [{Key, Value} | X].



filter_key(Key, Filter, Cfg) when erlang:is_atom(Key) ->
    case lists:keytake(Key, 1, Cfg) of
        {_, {_, Value}, Cfg3} ->
            case filter_value(Filter, Key, Value) of
                {ok, Value2} ->
                    {ok, Key, Value2, [], Cfg3};
                {ok, Value2, Unknown} ->
                    {ok, Key, Value2, Unknown, Cfg3};
                {error, ErrParams} ->
                    {
                        error,
                        #{
                            value => Value,
                            key => Key,
                            key_filter => Filter,
                            previous_error => ErrParams
                        }
                    }
            end;
        _ ->
            {error, #{key => Key, key_filter => Filter, reason => not_found}}
    end;

filter_key(FilterAsKey, Filter, Cfg) ->
    case filter([FilterAsKey], Cfg, [], [], 1) of
        {ok, [{_, Value}], Cfg2} when erlang:is_atom(Value) ->
            case filter_key(Value, Filter, Cfg2) of
                {_, ErrParams} ->
                    {error, ErrParams#{filtered_key => Value}};
                Ok ->
                    Ok
            end;
        {ok, [{_, Other}], _} ->
            {
                error,
                #{
                    key => FilterAsKey,
                    key_filter => Filter,
                    reason => bad_key,
                    filtered_key => Other
                }
            };
        {_, ErrParams} ->
            {
                error,
                #{
                    key => FilterAsKey,
                    key_filter => Filter,
                    previous_error => ErrParams
                }
            }
    end.


filter_value(Any, _, Value) when Any == any orelse Any == '_' ->
    {ok, Value};

filter_value(Type, _, Value) when Type == atom     orelse
                                  Type == binary   orelse
                                  Type == number   orelse
                                  Type == integer  orelse
                                  Type == float    orelse
                                  Type == list     orelse
                                  Type == boolean      ->
    case erlang:(
        erlang:list_to_atom("is_" ++ erlang:atom_to_list(Type))
    )(Value) of
        true ->
            {ok, Value};
        _ ->
            {error, #{allowed_type => Type}}
    end;

filter_value(proplist, _, Value) ->
    case cfg_utils:is_proplist(Value) of
        true ->
            {ok, Value};
        _ ->
            {error, #{allowed_type => proplist}}
    end;

filter_value(BIF, _, Value) when BIF == atom_to_list      orelse
                                 BIF == list_to_atom      orelse
                                 BIF == list_to_binary    orelse
                                 BIF == list_to_integer   orelse
                                 BIF == list_to_float     orelse
                                 BIF == binary_to_list    orelse
                                 BIF == binary_to_integer orelse
                                 BIF == binary_to_float       ->
    try
        {ok, erlang:BIF(Value)}
    catch
        _:_ ->
            {error, #{bif => BIF}}
    end;

filter_value(BIF, _, Value) when BIF == atom_to_binary orelse
                                 BIF == binary_to_atom     ->
    try
        {ok, erlang:BIF(Value, utf8)}
    catch
        _:_ ->
            {error, #{bif => BIF}}
    end;


filter_value(proplist_to_map, _, Value) ->
    case cfg_utils:is_proplist(Value) of
        true ->
            {ok, maps:from_list(Value)};
        _ ->
            {error, #{bif => proplist_to_map}}
    end;

filter_value({proplist, Filters}, Key, Value) ->
    case filter(Filters, Value, [], [], 1) of
        {Error, #{parent_keys := ParentKeys}=ErrParams} ->
            {Error, ErrParams#{parent_keys => [Key | ParentKeys]}};
        {Error, ErrParams} ->
            {Error, ErrParams#{parent_keys => [Key]}};
        Ok ->
            Ok
    end;

filter_value({list, Filter}, Key, Value) ->
    filter_list(Value, Filter, Key, [], []);

filter_value({And, List}, Key, Value) when And == '&' orelse And == 'and' ->
    filter_and(List, Key, Value, [], Value);

filter_value({Or, List}, Key, Value) when Or == '|' orelse Or == 'or' ->
    filter_or(List, Key, Value);

filter_value({one_of, List}, _, Value) ->
    filter_one_of(List, Value);

filter_value({size, Size}, _, Value) ->
    filter_size(Size, Value);

filter_value({mf, MF}, Keys, Value) ->
    filter_mf(MF, Keys, Value);

filter_value({f, F}, Keys, Value) ->
    filter_f(F, Keys, Value).


filter_mf({Mod, Func}, Keys, Value) when erlang:is_atom(Mod) andalso
                                         erlang:is_atom(Func)     ->
    Arity =
        case erlang:function_exported(Mod, Func, 2) of
            true ->
                2;
            _ ->
                case erlang:function_exported(Mod, Func, 1) of
                    true ->
                        1;
                    _ ->
                        0
                end
        end,
    case run_filter(Arity, {Mod, Func}, Keys, Value) of
        {error, ErrParams} ->
            {error, ErrParams#{module => Mod, function => Func}};
        Ok ->
            Ok
    end.


filter_f(Func, Keys, Value) when erlang:is_function(Func) ->
    {_, Arity} = erlang:fun_info(Func, arity),
    case run_filter(Arity, Func, Keys, Value) of
        {error, ErrParams} ->
            {error, ErrParams#{function => Func}};
        Ok ->
            Ok
    end.


run_filter(Arity, Filter, Key, Value) ->
    try
        case Filter of
            {Mod, Func} when Arity == 1 ->
                Mod:Func(Value);
            {Mod, Func} ->
                Mod:Func(Key, Value);
            _ when Arity == 1 ->
                Filter(Value);
            _ ->
                Filter(Key, Value)
        end
    of
        Ok when Ok == ok orelse Ok == true ->
            {ok, Value};
        {ok, _}=Ok ->
            Ok;
        false ->
            {error, #{reason => bad_value}};
        {error, {Reason, ErrParams}=Info} when erlang:is_atom(Reason) andalso
                                               erlang:is_map(ErrParams)    ->
            {error, #{previous_error => Info}};
        Other ->
            {error, #{returned_value => Other}}
    catch
        ?define_stacktrace(_, Reason, Stacktrace) ->
            {
                error,
                #{
                    exception => Reason,
                    stacktrace => ?get_stacktrace(Stacktrace)
                }
            }
    end.


filter_one_of(List, Value) ->
    case lists:member(Value, List) of
        true ->
            {ok, Value};
        _ ->
            {error, #{allowed_values => List}}
    end.


filter_size(Size, Value) when erlang:is_number(Size) ->
    case get_size(Value) of
        {ok, Size} -> % Size == Size
            {ok, Value};
        {ok, Size2} ->
            {error, #{allowed_size => Size, size => Size2}};
        Err ->
            Err
    end;

filter_size({min, Size}, Value) ->
    case get_size(Value) of
        {ok, Size2} when Size2 >= Size ->
            {ok, Value};
        {ok, Size2} ->
            {error, #{allowed_min_size => Size, size => Size2}};
        Err ->
            Err
    end;

filter_size({max, Size}, Value) ->
    case get_size(Value) of
        {ok, Size2} when Size2 =< Size ->
            {ok, Value};
        {ok, Size2} ->
            {error, #{allowed_max_size => Size, size => Size2}};
        Err ->
            Err
    end;

filter_size({MinSize, MaxSize}, Value) ->
    case get_size(Value) of
        {ok, Size2} when Size2 =< MaxSize andalso Size2 >= MinSize ->
            {ok, Value};
        {ok, Size2} when Size2 =< MaxSize ->
            {error, #{allowed_min_size => MinSize, size => Size2}};
        {ok, Size2} ->
            {error, #{allowed_max_size => MaxSize, size => Size2}};
        Err ->
            Err
    end.


get_size(Value) ->
    if
        erlang:is_list(Value) ->
            try 
                {ok, erlang:length(Value)}
            catch
                _:_ ->
                    {error, #{reason => unknown_size}}
            end;
        erlang:is_binary(Value) ->
            {ok, erlang:byte_size(Value)};
        erlang:is_number(Value) ->
            {ok, Value};
        true ->
            {error, #{reason => unknown_size}}
    end.


filter_and([Filter | Filters], Key, Value, Unknown, OrigValue) ->
    case filter_value(Filter, Key, Value) of
        {ok, Value2} ->
            filter_and(Filters, Key, Value2, Unknown, OrigValue);
        {ok, Value2, Unknown2} ->
            filter_and(Filters, Key, Value2, Unknown ++ Unknown2, OrigValue);
        {error, ErrParams} ->
            {
                error,
                #{
                    last_value => Value,
                    original_value => OrigValue,
                    previous_error => ErrParams
                }
            }
    end;

filter_and([], _, Value, Unknown, _) ->
    {ok, Value, Unknown}.


filter_or([Filter | Filters], Key, Value) ->
    case filter_value(Filter, Key, Value) of
        Ok when erlang:element(1, Ok) == ok ->
            Ok;
        Err ->
            if
                Filters == [] ->
                    Err;
                true ->
                    filter_or(Filters, Key, Value)
            end
    end.


filter_list([Element | List], Filter, Key, Unknown, Ret) ->
    case filter_value(Filter, Key, Element) of
        {ok, Element2, Unknown2} ->
            filter_list(
                List,
                Filter,
                Key,
                Unknown ++ Unknown2,
                [Element2 | Ret]
            );
        {ok, Element2} ->
            filter_list(List, Filter, Key, Unknown, [Element2 | Ret]);
        Err ->
            Err
    end;

filter_list([], _, _, Unknown, Ret) ->
    {ok, lists:reverse(Ret), Unknown}.


check_filters(AppName, _) when erlang:is_atom(AppName) ->
    case get_filters(AppName) of
        {ok, _} ->
            ok;
        Err ->
            Err
    end;

check_filters([Filter | Filters], Index) ->
    case check_filter(Filter) of
        ok ->
            check_filters(Filters, Index + 1);
        {_, ErrParams} ->
            {error, ErrParams#{index => Index}}
    end;

check_filters([], _) ->
    ok;

check_filters(_, 1) ->
    {error, #{}};

check_filters(Other, Index) ->
    {error, #{filter => Other, index => Index}}.


check_filter({Key, KeyFilter, Default}) ->
    case check_filter({Key, KeyFilter}) of
        {_, ErrParams} ->
            {
                error,
                ErrParams#{
                    key => Key,
                    key_filter => KeyFilter,
                    default_value => Default
                }
            };
        Ok ->
            Ok
    end;

check_filter({Key, KeyFilter}) ->
    case check_key(Key) of
        ok ->
            case check_key_filter(KeyFilter) of
                ok ->
                    ok;
                {_, ErrParams} ->
                    {error, ErrParams#{key => Key, key_filter => KeyFilter}}
            end;
        {_, ErrParams} ->
            {error, ErrParams#{key => Key, key_filter => KeyFilter}}
    end;

check_filter(Filter) ->
    {error, #{filter => Filter}}.


check_key(Key) when erlang:is_atom(Key) ->
    ok;

check_key(KeyFilter) ->
    case check_filter(KeyFilter) of
        ok ->
            ok;
        {_, ErrParams} ->
            {error, ErrParams#{key => KeyFilter}}
    end.


check_key_filter(X) when erlang:is_atom(X) ->
    case lists:member(
        X,
        [
            any,
            '_',
            atom,
            binary,
            number,
            integer,
            float,
            list,
            boolean,
            proplist,

            atom_to_list,
            list_to_atom,
            list_to_binary,
            list_to_integer,
            list_to_float,
            binary_to_list,
            binary_to_integer,
            binary_to_float,
            atom_to_binary,
            binary_to_atom,
            proplist_to_map
        ]
    ) of
        true ->
            ok;
        _ ->
            {error, #{reason => unknown_filter}}
    end;

check_key_filter({X, Filters}) when X == '&' orelse X == 'and' ->
    case Filters of
        [Filter | Filters2] ->
            case check_key_filter(Filter) of
                ok ->
                    check_key_filter({X, Filters2});
                {_, ErrParams} ->
                    {error, #{reason => bad_and, previous_error => ErrParams}}
            end;
        [] ->
            ok;
        _ ->
            {error, #{reason => bad_and}}
    end;

check_key_filter({X, Filters}) when X == '|' orelse X == 'or' ->
    case Filters of
        [Filter | Filters2] ->
            case check_key_filter(Filter) of
                ok ->
                    check_key_filter({X, Filters2});
                {_, ErrParams} ->
                    {error, #{reason => bad_or, previous_error => ErrParams}}
            end;
        [] ->
            ok;
        _ ->
            {error, #{reason => bad_or}}
    end;

check_key_filter({mf, MF}) ->
    case MF of
        {M, F} when erlang:is_atom(M) andalso erlang:is_atom(F) ->
            ok;
        _ ->
            {error, #{reason => bad_mf}}
    end;

check_key_filter({f, F}) ->
    if
        erlang:is_function(F, 1) orelse erlang:is_function(F, 2) ->
            ok;
        true ->
            {error, #{reason => bad_f}}
    end;

check_key_filter({proplist, Filters}) ->
    case check_filters(Filters, 1) of
        ok ->
            ok;
        {_, ErrParams} ->
            {error, #{reason => bad_proplist, previous_error => ErrParams}}
    end;

check_key_filter({list, Filters}) ->
    check_key_filter(Filters);

check_key_filter({size, Size}) ->
    case Size of
        _ when erlang:is_number(Size) ->
            ok;
        
        {X, Y} when (X == min orelse X == max) andalso erlang:is_number(Y) ->
            ok;
        
        {X, Y} when erlang:is_number(X) andalso
                    erlang:is_number(Y) andalso
                    X =< Y                   ->
            ok;
        _ ->
            {error, #{reason => bad_size}}
    end;

check_key_filter({one_of, List}) ->
    try erlang:length(List) of
        0 ->
            {error, #{reason => bad_one_of}};
        _ ->
            ok
    catch
        _:_ ->
            {error, #{reason => bad_one_of}}
    end;

check_key_filter(_) ->
    {error, #{reason => unknown_filter}}.


get_modules_filters([Mod | Mods], Filters) ->
    try erlang:function_exported(Mod, config_filters, 0) andalso
        Mod:config_filters()
    of
        false ->
            get_modules_filters(Mods, Filters);
        ModFilters ->
            case check(ModFilters) of
                ok ->
                    get_modules_filters(Mods, Filters ++ ModFilters);
                {_, Info} ->
                    {
                        error,
                        {
                            application_filters,
                            #{module => Mod, previous_error => Info}
                        }
                    }
            end
    catch
        ?define_stacktrace(_, Reason, Stacktrace) ->
            {
                error,
                {
                    application_filters
                    #{
                        module => Mod,
                        reason => Reason,
                        stacktrace => ?get_stacktrace(Stacktrace)
                    }
                }
            }
    end;

get_modules_filters(_, Filters) ->
    {ok, Filters}.
