%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg_utils).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% API:
-export([is_proplist/1, override/2, is_cfg/1]).

%% -----------------------------------------------------------------------------
%% API:

is_proplist([{Key, _}|Rest]) when erlang:is_atom(Key) ->
    is_proplist(Rest);

is_proplist([]) ->
    true;

is_proplist(_) ->
    false.


override(Cfg1, Cfg2) ->
    override(Cfg1, Cfg2, []).


override([{Key, Value1}=Item|Cfg1], Cfg2, Ret) ->
    case lists:keytake(Key, 1, Cfg2) of
        {_, {_, Value2}, Cfg3} ->
            Value3 =
                case {is_proplist(Value1), is_proplist(Value2)} of
                    {true, true} ->
                        override(Value1, Value2, []);
                    _ -> % {true, false} | {false, true} | {false, false}
                        Value2
                end,
            override(Cfg1, Cfg3, [{Key, Value3} | Ret]);
        _ ->
            override(Cfg1, Cfg2, [Item | Ret])
    end;

override(_, Cfg2, Ret) ->
    lists:reverse(Ret) ++ Cfg2.


is_cfg([{Key, Value} | Cfg]) ->
    if
        erlang:is_atom(Key) ->
            case check_value(Value) of
                ok ->
                    is_cfg(Cfg);
                Err ->
                    Err
            end;
        true ->
            {
                error,
                {
                    check_config,
                    #{
                        key => Key,
                        allowed_type => atom
                    }
                }
            }
    end;

is_cfg([]) ->
    ok;

is_cfg(Other) ->
    {
        error,
        {
            check_config,
            #{
                config => Other,
                allowed_type => proplist
            }
        }
    }.


check_value(List) when erlang:is_list(List) ->
    case is_cfg(List) of
        ok ->
            ok;
        _ -> % maybe list
            check_list(List)
    end;

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
