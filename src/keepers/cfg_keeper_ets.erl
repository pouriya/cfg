%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg_keeper_ets).
-behaviour(cfg_keeper).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% 'cfg_keeper' callbacks:
-export(
    [
        config_init/1,
        config_load/2,
        config_get/2,
        config_set/3,
        config_delete/2,
        config_delete/1
    ]
).

%% -----------------------------------------------------------------------------
%% 'cfg_keeper' callbacks:

config_init(Tab) when erlang:is_atom(Tab) ->
    ensure_table(Tab, [ordered_set]);

config_init({Tab, Opts}) when erlang:is_atom(Tab) andalso
                              erlang:is_list(Opts)     ->
    ensure_table(Tab, Opts).

config_load(Tab, Cfg) when erlang:is_atom(Tab) ->
    InsertFun =
        fun({Key, Value}) ->
            ets:insert(Tab, {Key, Value})
        end,
    lists:foreach(InsertFun, Cfg); %% ok

config_load({Tab, _}, Cfg) when erlang:is_atom(Tab) ->
    config_load(Tab, Cfg).


config_get(Tab, Key) when erlang:is_atom(Tab) ->
    case ets:lookup(Tab, Key) of
        [{_, Value}] ->
            {ok, Value};
        %% WARNING: This function assumes ETS set and ordered_set
        %% If you create a bag or duplicate_bag table, This pattern matching
        %% won't work and you will get 'not_found'
        _ ->
            not_found
    end;

config_get({Tab, _}, Key) when erlang:is_atom(Tab) ->
    config_get(Tab, Key).


config_set(Tab, Key, Value) when erlang:is_atom(Tab) ->
    _ = ets:insert(Tab, {Key, Value}),
    ok;

config_set({Tab, _}, Key, Value) when erlang:is_atom(Tab) ->
    config_set(Tab, Key, Value).


config_delete(Tab, Key) when erlang:is_atom(Tab) ->
    _ = ets:delete(Tab, Key),
    ok;

config_delete({Tab, _}, Key) when erlang:is_atom(Tab) ->
    config_delete(Tab, Key).


config_delete(Tab) when erlang:is_atom(Tab) ->
    _ = ets:delete_all_objects(Tab),
    ok;

config_delete({Tab, _}) when erlang:is_atom(Tab) ->
    config_delete(Tab).

%% -----------------------------------------------------------------------------
%% Internals:

ensure_table(Name, Opts) ->
    %% Add 'named_table' to options and remove duplicates:
    Opts2 = sets:to_list(sets:from_list([named_table | Opts])),
    try ets:new(Name, Opts2) of
        _ ->
            ok
    catch
        _:_ -> % badarg
            case is_table_usable(Name) of
                ok ->
                    ok;
                {_, ErrParams} ->
                    {error, {ets, ErrParams#{table => Name, options => Opts2}}};
                _ -> % not_found
                    %% wrong options:
                    {error, {ets, #{table => Name, options => Opts2}}}
            end
    end.


is_table_usable(Name) ->
    %% I got 'badarg' exception in above, So probably table is created before.
    try maps:from_list(ets:info(Name)) of
        #{owner := Owner} when Owner == erlang:self() ->
            ok;
        %% Owner is someone else:
        #{
            keypos := 1,
            protection := public,
            type := Type
        } when Type == set orelse Type == ordered_set ->
            ok;
        #{
            keypos := KeyPos,
            protection := Protection,
            type := Type,
            owner := Owner
        } ->
            {
                error,
                #{
                    table_info => #{
                        keypos => KeyPos,
                        protection => Protection,
                        type => Type,
                        owner => Owner,
                        caller => erlang:self()
                    }
                }
            }
    catch
        %% Table was not created and we had maps:from_list(undefined) and got 'badarg':
        _:_ ->
            not_found
    end.
