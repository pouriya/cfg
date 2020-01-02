%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg_reader_shell).
-behaviour(cfg_reader).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% 'config_reader' callback:
-export([read_config/1]).

%% -----------------------------------------------------------------------------
%% 'config_reader' callback:

read_config(Prefix) when erlang:is_list(Prefix) ->
    FilterFun =
        fun
            ({Key, Value}, Acc) ->
                case is_started_with(Prefix, Key) of
                    {ok, Key2} ->
                        [{string:to_lower(Key2), Value} | Acc];
                    _ ->
                        Acc
                end;
            
            (Env, Acc) ->
                case is_started_with(Prefix, Env) of
                    {ok, Rest} ->
                        [Key, Value] = string:split(Rest, "="),
                        [{string:to_lower(Key), Value} | Acc];
                    _ ->
                        Acc
                end
        end,
    Vars =
        try
            os:list_env_vars()
        catch
            _:_ -> % undef
                os:getenv()
        end,
    transform_shell_vars(lists:foldl(FilterFun, [], Vars), []).

%% -----------------------------------------------------------------------------
%% Internals:

is_started_with([X|RestX], [Y|RestY]) when X == Y ->
    is_started_with(RestX, RestY);

is_started_with([], [$_ |[_|_]=Y]) ->
    {ok, Y};

is_started_with([], [_|_]=Y) ->
    {ok, Y};

is_started_with(_, _) ->
    error.


transform_shell_vars([{Key, Value}|Env], Ret) ->
    case parse_key(Key, [], []) of
        {_, Keys} ->
            Value2 = transform_value(Value),
            case place_keys(Keys, Ret, Value2) of
                {ok, Ret2} ->
                    transform_shell_vars(Env, Ret2);
                Err ->
                    Err
            end;
        _ ->
            transform_shell_vars(Env, Ret)
    end;
transform_shell_vars(_, Ret) ->
    {ok, Ret}.


parse_key([$_, $_|_], [$_|_], _) -> % X___X
    error;

parse_key([Int|_], [], _) when Int >= $0 andalso Int =< $9 -> % starts with integer
    error;

parse_key([$_, $_|Rest], [_|_]=Buffer, Keys) ->
    parse_key(Rest, [], [to_atom(Buffer)|Keys]);

parse_key([Int|Rest], Buffer, Keys) when (Int >= $0 andalso Int =< $9) orelse
                                         (Int >= $a andalso Int =< $z) orelse
                                         Int == $_                         ->
    parse_key(Rest, [Int|Buffer], Keys);

parse_key([], [_|_]=Buffer, Keys)  ->
    {ok, lists:reverse([to_atom(Buffer)|Keys])};

parse_key([], _, Keys)  ->
    {ok, lists:reverse(Keys)};

parse_key(_, _, _)  ->
    error.


to_atom(Buffer) ->
    erlang:list_to_atom(lists:reverse(Buffer)).


place_keys([Key|Keys], List, Value) ->
    case lists:keytake(Key, 1, List) of
        {_, {_, List2}, List3} when erlang:is_list(List2) ->
            case place_keys(Keys, List2, Value) of
                {ok, Value2} ->
                    {ok, [{Key, Value2} | List3]};
                Err ->
                    Err
            end;
        {_, {_, Value2}, _} ->
            {
                error,
                {
                    shell,
                    #{key => Key, value_1 => Value, value_2 => Value2}
                }
            };
        _ ->
            case place_keys(Keys, [], Value) of
                {ok, Value2} ->
                    {ok, [{Key, Value2} | List]};
                Err ->
                    Err
            end
    end;

place_keys(_, _, Value) ->
    {ok, Value}.


transform_value([$"|[_|_]=Rest]=Value) ->
    case lists:reverse(Rest) of
        [$" | RevStr] ->
            erlang:list_to_binary(lists:reverse(RevStr));
        _ ->
            maybe_read_erlang_term(Value)
    end;

transform_value("") ->
    undefined;

transform_value(Value) ->
    maybe_read_erlang_term(Value).


maybe_read_erlang_term(Value) ->
    Value2 =
        case lists:last(Value) of
            $. ->
                Value;
            _ ->
                Value ++ "."
        end,
    case erl_scan:string(Value2) of
        {ok, ParsedStr, _} ->
            case erl_parse:parse_term(ParsedStr) of
                {ok, Value3} ->
                    Value3;
                _ ->
                    erlang:list_to_binary(Value)
            end;
        _ ->
            erlang:list_to_binary(Value)
    end.
