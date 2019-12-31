-module(cfg_server).
-author("pouriya").

%% API
-export([
    start_link/5,
    
    reload/1,
    
    subscribe/2,
    subscribe/3,
    unsubscribe/2,
    unsubscribe/3,
    
    change_options/2,
    set_readers/2,
    set_filters/2,
    set_keeper/2,
    
    info/1
]).


-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(S, state).
-record(?S, {readers, filters, keeper, config, subscribers, options}).

-define(O, options).
-record(?O, {
    error_unknown_config,
    notifier,
    notify_tag,
    change_priority,
    delete_on_terminate
}).

-define(RELOAD_TAG, reload).
-define(SUBSCRIBE_TAG, subscribe).
-define(UNSUBSCRIBE_TAG, unsubscribe).
-define(CHANGE_OPTIONS_TAG, change_options).
-define(SET_READERS_TAG, set_readers).
-define(SET_FILTERS_TAG, set_filters).
-define(SET_KEEPER_TAG, set_keeper).
-define(INFO_TAG, info).

start_link(RegisterName, Readers, Filters, Keeper, Opts) ->
    gen_server:start_link(
        RegisterName,
        ?MODULE,
        {Readers, Filters, Keeper, Opts},
        []
    ).


reload(Proc) ->
    gen_server:call(Proc, ?RELOAD_TAG, infinity).


subscribe(Proc, Keys) ->
    subscribe(Proc, Keys, erlang:self()).


subscribe(Proc, Key, Subscriber) when erlang:is_atom(Key) ->
    subscribe(Proc, [Key], Subscriber);

subscribe(Proc, Keys, Subscriber) when erlang:is_list(Keys) ->
    gen_server:call(Proc, {?SUBSCRIBE_TAG, Subscriber, Keys}).


unsubscribe(Proc, Keys) ->
    unsubscribe(Proc, Keys, erlang:self()).


unsubscribe(Proc, Key, Subscriber) when erlang:is_atom(Key) ->
    unsubscribe(Proc, [Key], Subscriber);

unsubscribe(Proc, Keys, Subscriber) when erlang:is_list(Keys) ->
    gen_server:call(Proc, {?UNSUBSCRIBE_TAG, Subscriber, Keys}).


change_options(Proc, Opts) when erlang:is_map(Opts) ->
    gen_server:call(Proc, {?CHANGE_OPTIONS_TAG, Opts}).


set_readers(Proc, Readers) ->
    gen_server:call(Proc, {?SET_READERS_TAG, Readers}).


set_filters(Proc, Filters) ->
    gen_server:call(Proc, {?SET_READERS_TAG, Filters}).


set_keeper(Proc, Keeper) ->
    gen_server:call(Proc, {?SET_READERS_TAG, Keeper}).


info(Proc) ->
    gen_server:call(Proc, ?INFO_TAG).


init({Readers, Filters, Keeper, Opts}) ->
    S = #?S{
        readers = Readers,
        filters = Filters,
        keeper = Keeper,
        subscribers = maps:get(subscribers, Opts, #{}),
        options = options(
            Opts,
            #?O{
                error_unknown_config = false,
                notifier = message,
                notify_tag = config,
                change_priority = false,
                delete_on_terminate = true
            }
        )
    },
    case load(S) of
        {ok, _}=Ok ->
            Ok;
        {_, Info} ->
            {stop, Info}
    end.


handle_call(?RELOAD_TAG, _, S) ->
    case do_reload(S) of
        {ok, S2} ->
            {reply, ok, S2};
        Err ->
            {reply, Err, S}
    end;

handle_call({?SUBSCRIBE_TAG, Subscriber, Keys}, _, S) ->
    {reply, ok, do_subscribe(S, Subscriber, Keys)};

handle_call({?UNSUBSCRIBE_TAG, Subscriber, Keys}, _, S) ->
    {reply, ok, do_unsubscribe(S, Subscriber, Keys)};

handle_call({?CHANGE_OPTIONS_TAG, Opts}, _, S) ->
    {reply, ok, S#?S{options = options(Opts, S#?S.options)}};


handle_call({?SET_READERS_TAG, Readers}, _, S) ->
    {reply, ok, S#?S{readers = Readers}};

handle_call({?SET_FILTERS_TAG, Filters}, _, S) ->
    {reply, ok, S#?S{filters =  Filters}};

handle_call({?SET_KEEPER_TAG, Keeper}, _, S) ->
    {reply, ok, S#?S{keeper =  Keeper}};

handle_call(
    ?INFO_TAG,
    _,
    #?S{
        readers = Readers,
        filters = Filters,
        keeper = Keeper,
        subscribers = Subscribers
    }=S
) ->
    {
        reply,
        #{
            readers => Readers,
            filters => Filters,
            keeper => Keeper,
            subscribers => Subscribers
        },
        S
    };

handle_call(Request, _, S) ->
    {reply, {error, {unknown, #{request => Request}}}, S}.


handle_cast(?RELOAD_TAG, S) ->
    case do_reload(S) of
        {ok, S2} ->
            {noreply, S2};
        {_, Info} ->
            {stop, Info, S}
    end;

handle_cast(_, S) ->
    {noreply, S}.


handle_info(_, S) ->
    {noreply, S}.


terminate(_, S) ->
    _ =
        if
            (S#?S.options)#?O.delete_on_terminate ->
                cfg:delete(S#?S.keeper);
            true ->
                ok
        end,
    ok.


options(#{error_unknown_config := Value}=Opts, Rec) ->
    options(
        maps:remove(error_unknown_config, Opts),
        Rec#?O{
            error_unknown_config =
                if
                    erlang:is_boolean(Value) ->
                        Value;
                    true ->
                        false
                end
        }
    );

options(#{notify_tag := Value}=Opts, Rec) ->
    options(
        maps:remove(notify_tag, Opts),
        Rec#?O{notify_tag = Value}
    );

options(#{delete_on_terminate := Value}=Opts, Rec) ->
    options(
        maps:remove(delete_on_terminate, Opts),
        Rec#?O{
            delete_on_terminate =
                if
                    erlang:is_boolean(Value) ->
                        Value;
                    true ->
                        true
                end
        }
    );

options(#{change_priority := Value}=Opts, Rec) ->
    options(
        maps:remove(change_priority, Opts),
        Rec#?O{
            change_priority =
                if
                    erlang:is_boolean(Value) ->
                        Value;
                    true ->
                        false
                end
        }
    );

options(#{notify := Value}=Opts, Rec) ->
    options(
        maps:remove(notify, Opts),
        Rec#?O{
            notifier =
                if
                    Value == call orelse Value == cast ->
                        Value;
                    true ->
                        message
                end
        }
    );

options(_, Rec) ->
    Rec.


read_and_filter(
    #?S{
        readers = Readers,
        filters = Filters,
        options = Opts
    }=S
) ->
    io:format("~p~n", [Readers]),
    case cfg:read(Readers) of
        {ok, Cfg} ->
            case cfg:filter(Cfg, Filters) of
                {ok, Cfg2, Unknown} ->
                    if
                        Opts#?O.error_unknown_config ->
                            {error, {unknown_config, #{values => Unknown}}};
                        true ->
                            {ok, S#?S{config = Cfg2}}
                    end;
                Err ->
                    Err
            end;
        Err ->
            Err
    end.


keep(#?S{keeper = Keeper, config = Cfg}=S) ->
    case cfg:keep(Cfg, Keeper) of
        ok ->
            {ok, S};
        Err ->
            Err
    end.


load(S) ->
    case read_and_filter(S) of
        {ok, S2} ->
            keep(S2);
        Err ->
            Err
    end.


do_reload(#?S{config = Cfg, options = Opts}=S) ->
    _ =
        if
            Opts#?O.change_priority ->
                erlang:process_flag(priority, high);
            true ->
                ok
        end,
    Result =
        case load(S) of
            {ok, #?S{config = Cfg2}=S2} ->
                _ = notify_subscribers(
                    maps:to_list(S#?S.subscribers),
                    Cfg,
                    Cfg2,
                    (S#?S.options)#?O.notifier,
                    (S#?S.options)#?O.notify_tag
                ),
                {ok, S2};
            Err ->
                Err
        end,
    _ =
        if
            Opts#?O.change_priority ->
                erlang:process_flag(priority, normal);
            true ->
                ok
        end,
    Result.


notify_subscribers(
    [{Subscriber, Keys} | Subscribers],
    OldCfg,
    NewCfg,
    Notifier,
    Tag
) ->
    _ = notify_subscriber(Keys, Subscriber, OldCfg, NewCfg, Notifier, Tag),
    notify_subscribers(Subscribers, OldCfg, NewCfg, Notifier, Tag);

notify_subscribers([Subscriber | Subscribers], OldCfg, NewCfg, Notifier, Tag) ->
    _ = notify_subscriber(Subscriber, config_reloaded, Notifier, Tag),
    notify_subscribers(Subscribers, OldCfg, NewCfg, Notifier, Tag);

notify_subscribers(_, _, _, _, _) ->
    ok.


notify_subscriber([Key | Keys], Subscriber, OldCfg, NewCfg, Notifier, Tag) ->
    _ = notify_subscriber(Key, Subscriber, OldCfg, NewCfg, Notifier, Tag),
    notify_subscriber(Keys, Subscriber, OldCfg, NewCfg, Notifier, Tag);

notify_subscriber(
    Key,
    Subscriber,
    OldCfg,
    NewCfg,
    Notifier,
    Tag
) when erlang:is_atom(Key) ->
    case {lists:keyfind(Key, 1, OldCfg), lists:keyfind(Key, 1, NewCfg)} of
        {X, X} -> % X == X
            ok;
        {{_, OldValue}, {_, NewValue}} ->
            notify_subscriber(
                Subscriber,
                {{value, OldValue}, {value, NewValue}},
                Notifier,
                Tag
            );
        {_, {_, NewValue}} ->
            notify_subscriber(
                Subscriber,
                {undefined, {value, NewValue}},
                Notifier,
                Tag
            );
        {{_, OldValue}, _} ->
            notify_subscriber(
                Subscriber,
                {{value, OldValue}, undefined},
                Notifier,
                Tag
            )
    end;

notify_subscriber(_, _, _, _, _, _) ->
    ok.


notify_subscriber(Subscriber, Msg, message, Tag) ->
    _ =
        try
            Subscriber ! {Tag, Msg}
        catch
            _:_ ->
                ok
        end,
    ok;

notify_subscriber(Subscriber, Msg, cast, Tag) ->
    _ =
        try
            gen_server:cast(Subscriber, {Tag, Msg})
        catch
            _:_ ->
                ok
        end,
    ok;

notify_subscriber(Subscriber, Msg, _, Tag) ->
    _ =
        try
            gen_server:call(Subscriber, {Tag, Msg}, infinity)
        catch
            _:_ ->
                ok
        end,
    ok.


do_subscribe(#?S{subscribers = Subscribers}=S, Subscriber, Keys) ->
    case maps:take(Subscriber, Subscribers) of
        {Keys, _} -> % Keys == Keys
            S;
        {Keys2, Subscribers2} ->
            S#?S{
                subscribers = Subscribers2#{
                    Subscriber => sets:to_list(
                        sets:from_list(Keys ++ Keys2)
                        )
                    }
            };
        _ ->
            S#?S{subscribers = Subscribers#{Subscriber => Keys}}
    end.


do_unsubscribe(#?S{subscribers = Subscribers}=S, Subscriber, Keys) ->
    case maps:take(Subscriber, Subscribers) of
        {Keys2, Subscribers2} when Keys == undefined orelse Keys == Keys2 ->
            S#?S{subscribers = Subscribers2};
        {Keys2, Subscribers2} ->
            S#?S{subscribers = Subscribers2#{Subscriber => Keys -- Keys2}};
        _ ->
            S
    end.
