-module(cfg_filter).

-include("cfg_stacktrace.hrl").

%% API
-export([do/2]).


do(Filters, Cfg) ->
    case filter(Filters, Cfg, [], []) of
        {_, ErrParams} ->
            {error, {filter_config, ErrParams#{filters => Filters}}};
        Ok ->
            Ok
    end.


filter([{Key, Filter, Default} | Filters], Cfg, Unknown, Ret) ->
    case lists:keytake(Key, 1, Cfg) of
        {_, {_, Value}, Cfg2} ->
            case filter_value(Filter, Key, Value) of
                {ok, Value2} ->
                    filter(Filters, Cfg2, Unknown, [{Key, Value2} | Ret]);
                {ok, Value2, Unknown2} ->
                    filter(
                        Filters,
                        Cfg2,
                        if
                            Unknown2 == [] ->
                                Unknown;
                            true ->
                                [{Key, Unknown2} | Unknown]
                        end,
                        [{Key, Value2} | Ret]
                    );
                {_, ErrParams} ->
                    {
                        error,
                        ErrParams#{
                            key => Key,
                            key_filter => Filter,
                            default_value => Default,
                            value => Value
                        }
                    }
            end;
        _ ->
            filter(Filters, Cfg, Unknown, [{Key, Default} | Ret])
    end;

filter([{Key, Filter} | Filters], Cfg, Unknown, Ret) ->
    case lists:keytake(Key, 1, Cfg) of
        {_, {_, Value}, Cfg2} ->
            case filter_value(Filter, Key, Value) of
                {ok, Value2} ->
                    filter(Filters, Cfg2, Unknown, [{Key, Value2} | Ret]);
                {ok, Value2, Unknown2} ->
                    filter(
                        Filters,
                        Cfg2,
                        if
                            Unknown2 == [] ->
                                Unknown;
                            true ->
                                [{Key, Unknown2} | Unknown]
                        end,
                        [{Key, Value2} | Ret]
                    );
                {_, ErrParams} ->
                    {
                        error,
                        ErrParams#{
                            key => Key,
                            key_filter => Filter,
                            value => Value
                        }
                    }
            end;
        _ ->
            {error, #{reason => not_found, key => Key, key_filter => Filter}}
    end;

filter([], Cfg, Unknown, Ret) ->
    {ok, lists:reverse(Ret), Cfg ++ Unknown};

filter([Filter | _], _, _, _) ->
    {error, #{filter => Filter}};

filter(Filters, _, _, _) ->
    {error, #{filters => Filters}}.


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
                                 BIF == binary_to_float ->
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

filter_value({proplist, Filters}, _, Value) ->
    filter(Filters, Value, [], []);

filter_value({list, Filter}, Key, Value) ->
    filter_list(Value, Filter, Key, [], []);

filter_value({And, List}, Key, Value) when And == '&' orelse And == 'and' ->
    filter_and(List, Key, Value, []);

filter_value({Or, List}, Key, Value) when Or == '|' orelse Or == 'or' ->
    filter_or(List, Key, Value);

filter_value({one_of, List}, _, Value) ->
    filter_one_of(List, Value);

filter_value({size, Size}, _, Value) ->
    filter_size(Size, Value);

filter_value({mf, MF}, Keys, Value) ->
    filter_mf(MF, Keys, Value);

filter_value({f, F}, Keys, Value) ->
    filter_f(F, Keys, Value);

filter_value(_, _, _) ->
    {error, #{reason => unknown_key_filter}}.


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
    end;

filter_mf(MF, _, _) ->
    {error, #{mf => MF, allowed_type => {atom, atom}}}.


filter_f(Func, Keys, Value) when erlang:is_function(Func) ->
    {_, Arity} = erlang:fun_info(Func, arity),
    case run_filter(Arity, Func, Keys, Value) of
        {error, ErrParams} ->
            {error, ErrParams#{function => Func}};
        Ok ->
            Ok
    end;

filter_f(MF, _, _) ->
    {error, #{f => MF, allowed_type => function}}.


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
    end;

run_filter(Arity, _, _, _) ->
    {error, #{arity => Arity, allowed_arity => [1, 2]}}.


filter_one_of(List, Value) ->
    try lists:member(Value, List) of
        true ->
            {ok, Value};
        _ ->
            {error, #{allowed_values => List}}
    catch
        _:_ ->
            {error, #{one_of => List, allowed_type => list}}
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

filter_size({MinSize, MaxSize}, Value) when erlang:is_number(MinSize) andalso
                                            erlang:is_number(MaxSize) andalso
                                            MaxSize > MinSize               ->
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

filter_size(Other, _) ->
    {
        error,
        #{
            size_value => Other,
            allowed_types => [
                number,
                {min, number},
                {max, number},
                {number, number}
            ]
        }
    }.


get_size(Value) ->
    if
        erlang:is_list(Value) ->
            try 
                {ok, erlang:length(Value)}
            catch
                _:_ ->
                    {error, #{allowed_types => [list, proplist, binary, number]}}
            end;
        erlang:is_binary(Value) ->
            {ok, erlang:byte_size(Value)};
        erlang:is_number(Value) ->
            {ok, Value};
        true ->
            {error, #{allowed_types => [list, proplist, binary, number]}}
    end.


filter_and([Filter | Filters], Key, Value, Unknown) ->
    case filter_value(Filter, Key, Value) of
        {ok, Value2} ->
            filter_and(Filters, Key, Value2, Unknown);
        {ok, Value2, Unknown2} ->
            filter_and(Filters, Key, Value2, Unknown ++ Unknown2);
        Err ->
            Err
    end;

filter_and([], _, Value, Unknown) ->
    {ok, Value, Unknown};

filter_and(_, _, _, _) ->
    {error, #{}}.


filter_or([Filter | Filters], Key, Value) ->
    case filter_value(Filter, Key, Value) of
        Ok when Ok == ok orelse erlang:element(1, Ok) == ok ->
            Ok;
        Err ->
            if
                Filters == [] ->
                    Err;
                true ->
                    filter_or(Filters, Key, Value)
            end
    end;

filter_or(_, _, _) ->
    {error, #{}}.


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
    {ok, lists:reverse(Ret), Unknown};

filter_list(Other, _, _, _, _) ->
    {error, #{list => Other}}.
