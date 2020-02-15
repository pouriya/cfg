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

        get_module_filters/1,
        get_application_filters/1
    ]
).

%% -----------------------------------------------------------------------------
%% Behaviour callback:

-callback
config_filters() -> Filters when
    Filters :: [] | [Filter],
    Filter :: {Key :: atom(), KeyFilter, DefaultValue :: term()}
            | {Key :: atom(), infer, DefaultValue :: term()}
            | {Key :: atom(), safe_infer, DefaultValue :: term()}
            | {Key :: atom(), KeyFilter},
    KeyFilter :: any | '_'
               | atom
               | binary
               | number
               | integer
               | float
               | list
               | boolean
               | proplist

               | try_atom
               | try_existing_atom
               | try_binary
               | try_number
               | try_integer
               | try_float
               | try_boolean
               | try_map

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

               | {'&' | 'and', KeyFilters}
               | {'|' | 'or', KeyFilters}

               | {proplist, Filters}
               | {list, KeyFilter}

               | {f, function()}
               | {mf, {module(), FunctionName :: atom()}}

               | {allowed_values, [any()]}

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
    case get_application_filters(AppName) of
        {ok, Filters} ->
            filter_application(Filters, Cfg, [], AppName);
        {_, {Reason, ErrParams}} ->
            {error, {Reason, ErrParams#{application => AppName}}}
    end;

do(Filters, Cfg) ->
    filter(Filters, Cfg).


filter(Filters, Cfg) ->
    case filter(Filters, Cfg, [], 1) of
        {_, {Reason, ErrParams}} ->
            {error, {Reason, ErrParams#{filters => Filters}}};
        Ok ->
            Ok
    end.


get_application_filters(AppName) ->
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


get_module_filters(Mod) when erlang:is_atom(Mod) ->
    try erlang:function_exported(Mod, config_filters, 0) andalso
        Mod:config_filters()
    of
        false ->
            {ok, []};
        Filters ->
            {ok, Filters}
    catch
        ?define_stacktrace(_, Reason, Stacktrace) ->
            {
                error,
                {
                    module_filters,
                    #{
                        module => Mod,
                        exception => Reason,
                        stacktrace => ?get_stacktrace(Stacktrace)
                    }
                }
            }
    end.

%% -----------------------------------------------------------------------------
%% Internals:

filter_application([{Mod, ModFilters} | Filters], Cfg, Ret, AppName) ->
    case filter(ModFilters, Cfg, Ret, 1) of
        {ok, Ret2} ->
            filter_application(Filters, Cfg, Ret2, AppName);
        {_, {Reason, ErrParams}} ->
            {
                error,
                {
                    Reason,
                    ErrParams#{filter_module => Mod, application => AppName}
                }
            }
    end;

filter_application(_, _, Ret, _) ->
    {ok, Ret}.


filter([Filter | Filters], Cfg, Ret, Index) ->
    case do_filter(Filter, Cfg) of
        {ok, {Key, Value}} ->
            filter(Filters, Cfg, [{Key, remove_readers(Value)}|Ret], Index + 1);
        {_, {Reason, ErrParams}} ->
            {error, {Reason, make_error(ErrParams#{index => Index})}}
    end;

filter([], _, Ret, _) ->
    {ok, lists:reverse(Ret)}.


do_filter({Key, KeyFilter, Default}=Filter, Cfg) ->
    case lookup_and_filter(Key, KeyFilter, {Default}, Cfg) of
        {ok, _}=Ok ->
            Ok;
        {_, {Reason, ErrParams}} ->
            {
                error,
                {
                    Reason,
                    ErrParams#{
                        filter => Filter,
                        key => Key,
                        key_filter => KeyFilter,
                        default_value => Default
                    }
                }
            }
    end;

do_filter({Key, KeyFilter}=Filter, Cfg) ->
    case lookup_and_filter(Key, KeyFilter, undefined, Cfg) of
        {ok, _}=Ok ->
            Ok;
        {_, {Reason, ErrParams}} ->
            {
                error,
                {
                    Reason,
                    ErrParams#{
                        filter => Filter,
                        key => Key,
                        key_filter => KeyFilter
                    }
                }
            }
    end;

do_filter(Unknown, _) ->
    {error, {filter_config, #{reason => bad_filter, filter => Unknown}}}.


lookup_and_filter(
    Key,
    Infer,
    {DefaultValue}=Default,
    Cfg
) when erlang:is_atom(Key)                    andalso
       (Infer == infer orelse Infer == safe_infer orelse Infer == infer_safe) ->
    SafeMode =
        if
            Infer == infer ->
                false;
            true ->
                true
        end,
    case infer_key_filter(DefaultValue, SafeMode) of
        {ok, KeyFilter} ->
            case lookup_and_filter(Key, KeyFilter, Default, Cfg) of
                {ok, _}=Ok ->
                    Ok;
                {_, {Reason, ErrParams}} ->
                    {
                        error,
                        {
                            Reason,
                            ErrParams#{infered_key_filter => KeyFilter}
                        }
                    }
            end;
        _ -> % error
            {error, {filter_config, #{reason => could_not_infer_filter}}}
    end;

lookup_and_filter(Key, KeyFilter, Default, Cfg) when erlang:is_atom(Key) ->
    case lookup(Key, Cfg, Default) of
        {ok, Value, Readers} ->
            case filter_value(KeyFilter, Key, Value) of
                {ok, Value2} ->
                    {ok, {Key, Value2}};
                {_, ErrParams} ->
                    {
                        error,
                        {
                            filter_config,
                            ErrParams#{readers => Readers, value => Value}
                        }
                    }
            end;
        not_found ->
            {error, {filter_config, #{reason => value_not_found}}}
    end;

lookup_and_filter(_, _, _, _) ->
    {error, {filter_config, #{reason => bad_key}}}.


lookup(Key, Cfg, Default) ->
    case lists:keyfind(Key, 1, Cfg) of
        {_, Value} ->
            {ok, Value, undefined};
        {_, Value, Readers} ->
            {ok, Value, Readers};
        _ when erlang:is_tuple(Default) ->
            {ok, erlang:element(1, Default), undefined};
        _ ->
            not_found
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
    case is_proplist(Value) of
        true ->
            {ok, Value};
        _ ->
            {error, #{allowed_type => proplist}}
    end;

filter_value(Type, _, Value) when Type == try_atom          orelse
                                  Type == try_existing_atom orelse
                                  Type == try_binary        orelse
                                  Type == try_number        orelse
                                  Type == try_integer       orelse
                                  Type == try_float         orelse
                                  Type == try_boolean       orelse
                                  Type == try_map               ->
    case try_convert(Type, Value) of
        {ok, _}=Ok ->
            Ok;
        not_found ->
            {error, #{reason => atom_not_found}};
        _ -> % error
            {error, #{reason => could_not_convert}}
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
            {error, #{reason => could_not_convert}}
    end;

filter_value(BIF, _, Value) when BIF == atom_to_binary orelse
                                 BIF == binary_to_atom     ->
    try
        {ok, erlang:BIF(Value, utf8)}
    catch
        _:_ ->
            {error, #{reason => could_not_convert}}
    end;

filter_value(proplist_to_map, _, Value) ->
    case is_proplist(Value) of
        true ->
            {ok, maps:from_list(remove_readers(Value))};
        _ ->
            {error, #{reason => could_not_convert}}
    end;

filter_value({proplist, Filters}, _, Value) ->
    case filter(Filters, Value, [], 1) of
        {ok, _}=Ok ->
            Ok;
        {_, {_, ErrParams}} ->
            {error, #{previous_error => ErrParams, proplist_filters => Filters}}
    end;

filter_value({list, Filter}, Key, Value) ->
    case filter_list(Value, Filter, Key, 1, []) of
        {ok, _}=Ok ->
            Ok;
        {_, ErrParams} ->
            {error, ErrParams#{list_filter => Filter}}
    end;

filter_value({And, List}, Key, Value) when And == '&' orelse And == 'and' ->
    case filter_and(List, Key, Value, 1, Value) of
        {ok, _}=Ok ->
            Ok;
        {_, ErrParams} ->
            {error, ErrParams#{and_filters => List}}
    end;

filter_value({Or, List}, Key, Value) when Or == '|' orelse Or == 'or' ->
    case filter_or(List, Key, Value, 0, undefined, undefined) of
        {ok, _}=Ok ->
            Ok;
        {_, ErrParams} ->
            {error, ErrParams#{or_filters => List}}
    end;

filter_value({allowed_values, List}, _, Value) ->
    filter_allowed_values(List, Value);

filter_value({size, Size}, _, Value) ->
    filter_size(Size, Value);

filter_value({mf, MF}, Keys, Value) ->
    filter_mf(MF, Keys, Value);

filter_value({f, F}, Keys, Value) ->
    filter_f(F, Keys, Value);

filter_value(_, _, _) ->
    {error, #{reason => bad_key_filter}}.


filter_mf({Mod, Func}, Keys, Value) when erlang:is_atom(Mod) andalso
                                         erlang:is_atom(Func)     ->
    Arity =
        case erlang:function_exported(Mod, Func, 2) of
            true ->
                2;
            _ ->
                1
        end,
    case run_filter(Arity, {Mod, Func}, Keys, Value) of
        {error, ErrParams} ->
            {
                error,
                ErrParams#{
                    mf_module => Mod,
                    mf_function => Func,
                    mf_arity => Arity
                }
            };
        Ok ->
            Ok
    end.


filter_f(Func, Keys, Value) when erlang:is_function(Func) ->
    {_, Arity} = erlang:fun_info(Func, arity),
    case run_filter(Arity, Func, Keys, Value) of
        {error, ErrParams} ->
            {error, ErrParams#{f_function => Func, f_arity => Arity}};
        Ok ->
            Ok
    end.


run_filter(Arity, Filter, Key, Value) when Arity == 1 orelse Arity == 2 ->
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
        {error, ErrParams} when erlang:is_map(ErrParams) ->
            {error, #{previous_error => ErrParams}};
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
    end;

run_filter(_, _, _, _) ->
    {error, #{f_allowed_arity => [1,2]}}.


filter_allowed_values(List, Value) ->
    try lists:member(Value, List) of
        true ->
            {ok, Value};
        _ ->
            {error, #{allowed_values => List}}
    catch
        _:_ ->
            {error, #{reason => bad_allowed_values, allowed_values => List}}
    end.


filter_size(Size, Value) when erlang:is_number(Size) ->
    case get_size(Value) of
        {ok, Size2} when erlang:round(Size2) == Size ->
            {ok, Value};
        {ok, Size2} ->
            {error, #{allowed_size => Size, size => Size2}};
        Err ->
            Err
    end;

filter_size({min, Size}, Value) when erlang:is_number(Size) ->
    case get_size(Value) of
        {ok, Size2} when Size2 >= Size ->
            {ok, Value};
        {ok, Size2} ->
            {error, #{allowed_min_size => Size, size => Size2}};
        Err ->
            Err
    end;

filter_size({max, Size}, Value) when erlang:is_number(Size) ->
    case get_size(Value) of
        {ok, Size2} when Size2 =< Size ->
            {ok, Value};
        {ok, Size2} ->
            {error, #{allowed_max_size => Size, size => Size2}};
        Err ->
            Err
    end;

filter_size(
    {MinSize, MaxSize},
    Value
) when erlang:is_number(MinSize) andalso erlang:is_number(MaxSize) ->
    case get_size(Value) of
        {ok, Size2} when Size2 =< MaxSize andalso Size2 >= MinSize ->
            {ok, Value};
        {ok, Size2} when Size2 =< MaxSize ->
            {error, #{allowed_min_size => MinSize, size => Size2}};
        {ok, Size2} ->
            {error, #{allowed_max_size => MaxSize, size => Size2}};
        Err ->
            Err
    end;

filter_size(Unknown, _) ->
    {error, #{reason => size_bad_value, size_value => Unknown}}.


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
        erlang:is_integer(Value) andalso Value >= 0 ->
            {ok, Value};
        erlang:is_float(Value) andalso Value >= 0 ->
            {ok, erlang:trunc(Value)};
        erlang:is_atom(Value) ->
            {ok, erlang:length(erlang:atom_to_list(Value))};
        true ->
            {error, #{reason => unknown_size}}
    end.


filter_and([Filter | Filters], Key, Value, Index, OrigValue) ->
    case filter_value(Filter, Key, Value) of
        {ok, Value2} ->
            filter_and(Filters, Key, Value2, Index + 1, OrigValue);
        {_, ErrParams} ->
            {
                error,
                #{
                    and_last_value => Value,
                    and_original_value => OrigValue,
                    previous_error => ErrParams,
                    and_filter => Filter,
                    and_filter_index => Index
                }
            }
    end;

filter_and([], _, Value, _, _) ->
    {ok, Value};

filter_and(Filters, _, _, 1, _) ->
    {error, #{reason => and_bad_filters, and_filters => Filters}};

filter_and(Unknown, _, _, Index, _) ->
    {
        error,
        #{
            reason => and_bad_filter,
            and_filter_index => Index,
            and_filter => Unknown
        }
    }.


filter_or([Filter | Filters], Key, Value, Index, _, _) ->
    case filter_value(Filter, Key, Value) of
        {ok, _}=Ok ->
            Ok;
        {_, ErrParams} ->
            filter_or(Filters, Key, Value, Index + 1, Filter, ErrParams)
    end;

filter_or([], _, _, 0, _, _) ->
    {error, #{reason => or_empty_filters}};

filter_or([], _, _, Index, LastFilter, LastErrParams) ->
    {
        error,
        #{
            or_filter_index => Index,
            or_last_filter => LastFilter,
            previous_error => LastErrParams
        }
    };

filter_or(Filters, _, _, 0, _, _) ->
    {error, #{reason => or_bad_filters, or_filters => Filters}};

filter_or(Unknown, _, _, Index, _, _) ->
    {
        error,
        #{
            reason => or_bad_filter,
            or_filter_index => Index,
            or_filter => Unknown
        }
    }.


filter_list([Element | List], Filter, Key, Index, Ret) ->
    case filter_value(Filter, Key, Element) of
        {ok, Element2} ->
            filter_list(List, Filter, Key, Index + 1, [Element2 | Ret]);
        {_, ErrParams} ->
            {
                error,
                #{
                    list_element => Element,
                    list_index => Index,
                    previous_error => ErrParams
                }
            }
    end;

filter_list([], _, _, _, Ret) ->
    {ok, lists:reverse(Ret)};

filter_list(_, _, _, 1, _) ->
    {error, #{allowed_type => list}};

filter_list(Unknown, _, _, Index, _) ->
    {
        error,
        #{
            reason => bad_list_element,
            index => Index,
            list_element => Unknown
        }
    }.



get_modules_filters([Mod | Mods], Filters) ->
    case get_module_filters(Mod) of
        {ok, ModFilters} ->
            get_modules_filters(Mods, [{Mod, ModFilters} | Filters]);
        Err ->
            Err
    end;

get_modules_filters(_, Filters) ->
    {ok, Filters}.


infer_key_filter(Value, SafeMode) when erlang:is_atom(Value) ->
    if
        SafeMode ->
            {ok, try_existing_atom};
        true ->
            {ok, try_atom}
    end;

infer_key_filter(Value, _) when erlang:is_binary(Value) ->
    {ok, try_binary};

infer_key_filter(Value, _) when erlang:is_integer(Value) ->
    {ok, try_integer};

infer_key_filter(Value, _) when erlang:is_float(Value) ->
    {ok, try_float};

infer_key_filter(Value, SafeMode) when erlang:is_list(Value) ->
    case is_proplist(Value) of
        true ->
            infer_proplist(Value, SafeMode, []);
        _ ->
            infer_list(Value, SafeMode, [])
    end;

infer_key_filter(Value, SafeMode) when erlang:is_map(Value) ->
    case infer_key_filter(maps:to_list(Value), SafeMode) of
        {ok, ProplistFilter} ->
            {ok, {'and', [ProplistFilter, proplist_to_map]}};
        Err ->
            Err
    end;

infer_key_filter(_, _) ->
    error.


infer_proplist([{Key, Value} | Proplist], SafeMode, Ret) ->
    case infer_key_filter(Value, SafeMode) of
        {ok, Filter} ->
            infer_proplist(Proplist, SafeMode, [{Key, Filter, Value} | Ret]);
        Err ->
            Err
    end;

infer_proplist([], _, [_|_]=Ret) ->
    {ok, {proplist, lists:reverse(Ret)}};

infer_proplist([], _, _) ->
    {ok, list};

infer_proplist(_, _, _) ->
    error.


infer_list([Value | List], SafeMode, Ret) ->
    case infer_key_filter(Value, SafeMode) of
        {ok, Filter} ->
            infer_list(List, SafeMode, [Filter | Ret]);
        Err ->
            Err
    end;

infer_list(_, _, [_|_]=Filters) ->
    {ok, {list, {'or', sets:to_list(sets:from_list(lists:reverse(Filters)))}}};

infer_list(_, _, _) ->
    {ok, list}.



try_convert(try_atom, Value) ->
    try_convert_to_atom(Value);

try_convert(try_binary, Value) ->
    try_convert_to_binary(Value);

try_convert(try_integer, Value) ->
    try_convert_to_integer(Value);

try_convert(try_number, Value) ->
    try_convert_to_number(Value);

try_convert(try_float, Value) ->
    try_convert_to_float(Value);

try_convert(try_existing_atom, Value) ->
    try_convert_to_existing_atom(Value);

try_convert(try_boolean, Value) ->
    try_convert_to_boolean(Value);

try_convert(_, Value) -> % try_map
    try_convert_to_map(Value).


try_convert_to_atom(Value) when erlang:is_binary(Value) ->
    {ok, erlang:binary_to_atom(Value, utf8)};

try_convert_to_atom(Value) when erlang:is_list(Value) ->
    try
        {ok, erlang:list_to_atom(Value)}
    catch
        _:_ ->
            error
    end;

try_convert_to_atom(Value) when erlang:is_integer(Value) ->
    {ok, erlang:list_to_atom(erlang:integer_to_list(Value))};

try_convert_to_atom(Value) when erlang:is_atom(Value) ->
    {ok, Value};

try_convert_to_atom(_) ->
    error.


try_convert_to_existing_atom(Value) when erlang:is_binary(Value) ->
    try_convert_to_existing_atom(erlang:binary_to_list(Value));

try_convert_to_existing_atom(Value) when erlang:is_list(Value) ->
    try
        {ok, erlang:list_to_existing_atom(Value)}
    catch
        _:_ ->
            not_found
    end;

try_convert_to_existing_atom(Value) when erlang:is_integer(Value) ->
    try_convert_to_existing_atom(erlang:integer_to_list(Value));

try_convert_to_existing_atom(Value) when erlang:is_atom(Value) ->
    {ok, Value};

try_convert_to_existing_atom(_) ->
    error.

try_convert_to_binary(Value) when erlang:is_list(Value) ->
    try
        {ok, erlang:list_to_binary(Value)}
    catch
        _:_ ->
            error
    end;

try_convert_to_binary(Value) when erlang:is_integer(Value) ->
    {ok, erlang:integer_to_binary(Value)};

try_convert_to_binary(Value) when erlang:is_float(Value) ->
    {ok, erlang:float_to_binary(Value, [compact, {decimals, 15}])};

try_convert_to_binary(Value) when erlang:is_atom(Value) ->
    {ok, erlang:atom_to_binary(Value, utf8)};

try_convert_to_binary(Value) when erlang:is_binary(Value) ->
    {ok, Value};

try_convert_to_binary(_) ->
    error.


try_convert_to_integer(Value) when erlang:is_binary(Value) ->
    try_convert_to_integer(erlang:binary_to_list(Value));

try_convert_to_integer(Value) when erlang:is_list(Value) ->
    TryFloat =
        try erlang:list_to_float(Value) of
            Value2 ->
                try_convert_to_integer(Value2)
        catch
            _:_ ->
                error
        end,
    if
        TryFloat /= error ->
            TryFloat;
        true ->
            try
                {ok, erlang:list_to_integer(Value)}
            catch
                _:_ ->
                    error
            end
    end;

try_convert_to_integer(Value) when erlang:is_float(Value) ->
    {ok, erlang:list_to_integer(erlang:float_to_list(Value, [{decimals, 0}]))};

try_convert_to_integer(Value) when erlang:is_atom(Value) ->
    try_convert_to_integer(erlang:atom_to_list(Value));

try_convert_to_integer(Value) when erlang:is_integer(Value) ->
    {ok, Value};

try_convert_to_integer(_) ->
    error.


try_convert_to_float(Value) when erlang:is_binary(Value) ->
    try_convert_to_float(erlang:binary_to_list(Value));

try_convert_to_float(Value) when erlang:is_list(Value) ->
    TryFloat =
        try
            {ok, erlang:list_to_float(Value)}
        catch
            _:_ ->
                error
        end,
    if
        TryFloat /= error ->
            TryFloat;
        true ->
            try
                {ok, erlang:float(erlang:list_to_integer(Value))}
            catch
                _:_ ->
                    error
            end
    end;

try_convert_to_float(Value) when erlang:is_integer(Value) ->
    {ok, erlang:float(Value)};

try_convert_to_float(Value) when erlang:is_atom(Value) ->
    try_convert_to_float(erlang:atom_to_list(Value));

try_convert_to_float(Value) when erlang:is_float(Value) ->
    {ok, Value};

try_convert_to_float(_) ->
    error.


try_convert_to_number(Value) ->
    case {try_convert_to_float(Value), try_convert_to_integer(Value)} of
        {{_, Float}, {_, Integer}=Ok} when Float == Integer ->
            Ok;
        {{_, _}=Ok, _} ->
            Ok;
        _ ->
            error
    end.

try_convert_to_boolean(Value) when erlang:is_binary(Value) ->
    try_convert_to_boolean(erlang:binary_to_list(Value));

try_convert_to_boolean("true") ->
    {ok, true};

try_convert_to_boolean("false") ->
    {ok, false};

try_convert_to_boolean(0) ->
    {ok, false};

try_convert_to_boolean(1) ->
    {ok, true};

try_convert_to_boolean(Value) when erlang:is_list(Value) ->
    case try_convert_to_integer(Value) of
        {ok, Integer} ->
            try_convert_to_boolean(Integer);
        _ ->
            error
    end;

try_convert_to_boolean(Value) when erlang:is_boolean(Value) ->
    {ok, Value};

try_convert_to_boolean(_) ->
    error.


try_convert_to_map(Value) ->
    case is_proplist(Value) of
        true ->
            {ok, maps:from_list(remove_readers(Value))};
        _ ->
            error
    end.

is_proplist([{Key, _}|Rest]) when erlang:is_atom(Key) ->
    is_proplist(Rest);

is_proplist([{Key, _, _}|Rest]) when erlang:is_atom(Key) ->
    is_proplist(Rest);

is_proplist([]) ->
    true;

is_proplist(_) ->
    false.


remove_readers([{Key, Value, _} | Rest]) ->
    [{Key, remove_readers(Value)} | remove_readers(Rest)];

remove_readers([{Key, Value} | Rest]) ->
    [{Key, remove_readers(Value)} | remove_readers(Rest)];

remove_readers(X) ->
    X.


make_error(
    #{previous_error := PrevErr}=ErrParams
) when erlang:is_map(PrevErr) ->
    PrevErr2 = make_error(PrevErr),
    FilterFun =
        fun(Key, Value, {PrevErrX, ErrParamsX}) ->
            case maps:is_key(Key, ErrParamsX) of
                false ->
                    {maps:remove(Key, PrevErrX), ErrParamsX#{Key => Value}};
                _ ->
                    {PrevErrX, ErrParamsX}
            end
        end,
    case maps:fold(FilterFun, {make_error(PrevErr), ErrParams}, PrevErr2) of
        {PrevErr3, ErrParams2} when erlang:map_size(PrevErr3) == 0 ->
            maps:remove(previous_error, ErrParams2);
        {PrevErr3, ErrParams2} ->
            ErrParams2#{previous_error => PrevErr3}
    end;

make_error(X) ->
    X.
