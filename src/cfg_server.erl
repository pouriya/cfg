%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg_server).
-behaviour(gen_server).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% API
-export([
    start_link/5,
    
    reload/1,
    async_reload/1,
    
    subscribe/2,
    subscribe/3,
    unsubscribe/1,
    unsubscribe/2,
    
    change_options/2,
    set_readers/2,
    set_filters/2,
    set_keeper/2,
    
    info/1,
    stop/1
]).

%% 'gen_server' & 'gen_event' callbacks:
-export([
    init/1,
    handle_info/2,
    terminate/2
]).

%% 'gen_server' callbacks:
-export([
    handle_call/3,
    handle_cast/2
]).


%% 'gen_event' callbacks:
-export([
    handle_call/2,
    handle_event/2
]).

%% Local exports:
-export([
    default_pre_reload/0,
    default_post_reload/1
]).

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(S, state).
-record(
    ?S,
    {
        readers,
        filters,
        keeper,
        config,
        filtered_config,
        subscribers,
        options,
        register_name,
        notify_method,
        notify_tag,
        change_priority,
        delete_on_terminate,
        reload_on_signal,
        signal_handler_state,
        signal_handler_pre_reload,
        signal_handler_post_reload,
        signal_handler_reload_sync,
        reload_on_timer,
        timer_state,
        timer_pre_reload,
        timer_post_reload
    }
).

-define(DEFAULT_NOTIFY_TAG, config).
-define(DEFAULT_NOTIFY_METHOD, message).
-define(DEFAULT_CHANGE_PRIORITY, false).
-define(DEFAULT_DELETE_ON_TERMINATE, true).
-define(DEFAULT_RELOAD_ON_SIGNAL, false).
-define(DEFAULT_SIGNAL_HANDLER_PRE_RELOAD, fun ?MODULE:default_pre_reload/0).
-define(DEFAULT_SIGNAL_HANDLER_POST_RELOAD, fun ?MODULE:default_post_reload/1).
-define(DEFAULT_SIGNAL_HANDLER_RELOAD_SYNC, true).
-define(DEFAULT_RELOAD_ON_TIMER, false).
-define(DEFAULT_TIMER_PRE_RELOAD, fun ?MODULE:default_pre_reload/0).
-define(DEFAULT_TIMER_POST_RELOAD, fun ?MODULE:default_post_reload/1).

-define(DEFAULT_SIGNAL_NAME, sighup).
-define(DEFAULT_TIMER_VALUE, 1). % 1m

-define(SHS, signal_handler_state).
-record(
    ?SHS,
    {
        signal,
        register_name,
        pre_reload_function,
        post_reload_function,
        sync_reload
    }
).

-define(RELOAD_TAG, reload).
-define(SUBSCRIBE_TAG, subscribe).
-define(UNSUBSCRIBE_TAG, unsubscribe).
-define(CHANGE_OPTIONS_TAG, change_options).
-define(SET_READERS_TAG, set_readers).
-define(SET_FILTERS_TAG, set_filters).
-define(SET_KEEPER_TAG, set_keeper).
-define(INFO_TAG, info).
-define(TIMEOUT_TAG, timeout).
-define(STOP_TAG, stop).

-include("cfg_stacktrace.hrl").

%% -----------------------------------------------------------------------------
%% API:

start_link(RegisterName, Readers, Filters, Keeper, Opts) ->
    gen_server:start_link(
        RegisterName,
        ?MODULE,
        {RegisterName, Readers, Filters, Keeper, Opts},
        []
    ).


reload(Proc) ->
    gen_server:call(Proc, ?RELOAD_TAG, infinity).


async_reload(Proc) ->
    gen_server:cast(Proc, ?RELOAD_TAG).


subscribe(Proc, Keys) ->
    subscribe(Proc, Keys, erlang:self()).


subscribe(Proc, Filters, Subscriber) ->
    gen_server:call(Proc, {?SUBSCRIBE_TAG, Subscriber, Filters}, infinity).


unsubscribe(Proc) ->
    unsubscribe(Proc, erlang:self()).


unsubscribe(Proc, Subscriber) ->
    gen_server:call(Proc, {?UNSUBSCRIBE_TAG, Subscriber}).


change_options(Proc, Opts) when erlang:is_map(Opts) ->
    gen_server:call(Proc, {?CHANGE_OPTIONS_TAG, Opts}).


set_readers(Proc, Readers) ->
    gen_server:call(Proc, {?SET_READERS_TAG, Readers}).


set_filters(Proc, Filters) ->
    gen_server:call(Proc, {?SET_FILTERS_TAG, Filters}).


set_keeper(Proc, Keeper) ->
    gen_server:call(Proc, {?SET_KEEPER_TAG, Keeper}).


info(Proc) ->
    gen_server:call(Proc, ?INFO_TAG).


stop(Server) ->
    gen_server:stop(Server).

%% -----------------------------------------------------------------------------
%% 'gen_server' & 'gen_event' callbacks:

%% 'gen_event':
init(#?SHS{}=S) ->
    {ok, S};
init({#?SHS{}=S, _}) ->
    {ok, S};
%% 'gen_server':
init({RegisterName, Readers, Filters, Keeper, Opts}) ->
    S = options(
        Opts,
        #?S{
            readers = Readers,
            filters = Filters,
            keeper = Keeper,
            register_name = RegisterName,
            signal_handler_state = undefined,
            timer_state = undefined,

            subscribers = #{},
            notify_method = ?DEFAULT_NOTIFY_METHOD,
            notify_tag = ?DEFAULT_NOTIFY_TAG,
            change_priority = ?DEFAULT_CHANGE_PRIORITY,
            delete_on_terminate = ?DEFAULT_DELETE_ON_TERMINATE,
            reload_on_signal = ?DEFAULT_RELOAD_ON_SIGNAL,
            signal_handler_pre_reload = ?DEFAULT_SIGNAL_HANDLER_PRE_RELOAD,
            signal_handler_post_reload = ?DEFAULT_SIGNAL_HANDLER_POST_RELOAD,
            signal_handler_reload_sync = ?DEFAULT_SIGNAL_HANDLER_RELOAD_SYNC,
            reload_on_timer = ?DEFAULT_RELOAD_ON_TIMER,
            timer_pre_reload = ?DEFAULT_TIMER_PRE_RELOAD,
            timer_post_reload = ?DEFAULT_TIMER_POST_RELOAD
        }
    ),
    case load(S) of
        {ok, _}=Ok ->
            Ok;
        {_, Info} ->
            {stop, Info}
    end.


%% 'gen_server':
handle_info(
    ?TIMEOUT_TAG,
    #?S{
        timer_state = TimerRef,
        timer_pre_reload = PreReload,
        timer_post_reload = PostReload
    }=S
) when TimerRef /= undefined ->
    try PreReload() of
        ok ->
            {Result, S2} =
                case do_reload(S) of
                    {error, _}=Err ->
                        {Err, S};
                    Ok ->
                        Ok
                end,
            try PostReload(Result) of
                {error, Reason} ->
                    {stop, Reason, S2};
                _ ->
                    {
                        noreply,
                        S#?S{
                            timer_state = erlang:send_after(
                                S#?S.reload_on_timer,
                                erlang:self(),
                                ?TIMEOUT_TAG,
                                []
                            )
                        }
                    }
            catch
                ?define_stacktrace(_, Reason, Stacktrace) ->
                    {
                        stop,
                        {
                            timer_post_reload,
                            #{
                                reason => Reason,
                                stacktrace => ?get_stacktrace(Stacktrace)
                            }
                        },
                        S
                    }
            end;
        _ ->
            {noreply, S#?S{timer_state = undefined}}
    catch
        ?define_stacktrace(_, Reason, Stacktrace) ->
            {
                stop,
                {
                    timer_pre_reload,
                    #{
                        reason => Reason,
                        stacktrace => ?get_stacktrace(Stacktrace)
                    }
                },
                S
            }
    end;

handle_info(_, #?S{}=S) ->
    {noreply, S};
%% 'gen_event':
handle_info(_, S) ->
    {ok, S}.

%% 'gen_server':
terminate(_, #?S{}=S) ->
    _ =
        if
            S#?S.delete_on_terminate ->
                cfg:delete(S#?S.keeper);
            true ->
                ok
        end,
    ok;
%% 'gen_event':
terminate(_, _) ->
    ok.

%% -----------------------------------------------------------------------------
%% 'gen_server' callbacks:

handle_call(?RELOAD_TAG, _, S) ->
    case do_reload(S) of
        {ok, S2} ->
            {reply, ok, S2};
        Err ->
            {reply, Err, S}
    end;

handle_call({?SUBSCRIBE_TAG, Subscriber, Keys}, _, S) ->
    {reply, ok, do_subscribe(S, Subscriber, Keys)};

handle_call({?UNSUBSCRIBE_TAG, Subscriber}, _, S) ->
    {reply, ok, do_unsubscribe(S, Subscriber)};

handle_call({?CHANGE_OPTIONS_TAG, Opts}, _, S) ->
    {reply, ok, options(Opts, S)};

handle_call({?SET_READERS_TAG, Readers}, _, S) ->
    {reply, ok, S#?S{readers = Readers}};

handle_call({?SET_FILTERS_TAG, Filters}, _, S) ->
    {reply, ok, S#?S{filters =  Filters}};

handle_call({?SET_KEEPER_TAG, X}, _, #?S{keeper = X}=S) ->
    {reply, ok, S};

handle_call({?SET_KEEPER_TAG, NewKeeper}, _, #?S{keeper = OldKeeper}=S) ->
    case keep(S#?S{keeper = NewKeeper}) of
        {ok, S2} ->
            _ =
                if
                    S#?S.delete_on_terminate ->
                        cfg:delete(OldKeeper);
                    true ->
                        ok
                end,
            {reply, ok, S2};
        Err ->
            {reply, Err, S}
    end;

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

handle_cast({?STOP_TAG, Info}, #?S{}=S) ->
    {stop, Info, S};

handle_cast(_, S) ->
    {noreply, S}.

%% -----------------------------------------------------------------------------
%% 'gen_event' callbacks:

handle_event(
    Signal,
    #?SHS{
        register_name = RegisterName,
        signal = Signal,
        pre_reload_function = PreReload,
        post_reload_function =  PostReload,
        sync_reload = SyncReload
    }=S
) -> % Signal == Signal
    CallName =
        case RegisterName of
            {local, LocalName} ->
                LocalName;
            _ ->
                RegisterName
        end,
    Result =
        try PreReload() of
            ok ->
                ReloadResult =
                    try
                        if
                            SyncReload ->
                                reload(CallName);
                            true ->
                                async_reload(CallName)
                        end % ok
                    catch
                        ?define_stacktrace(_, Reason, Stacktrace) ->
                            {
                                error,
                                {
                                    reload_config,
                                    #{
                                        reason => Reason,
                                        stacktrace => ?get_stacktrace(Stacktrace),
                                        register_name => RegisterName,
                                        signal => Signal
                                    }
                                }
                            }
                    end,
                try
                    PostReload(ReloadResult)
                catch
                    ?define_stacktrace(_, Reason2, Stacktrace2) ->
                        {
                            error,
                            {
                                signal_handler_pre_reload,
                                #{
                                    reason => Reason2,
                                    stacktrace => ?get_stacktrace(Stacktrace2),
                                    signal => Signal
                                }
                            }
                        }
                end;
            _ ->
                ok
        catch
            ?define_stacktrace(_, Reason, Stacktrace) ->
                {
                    error,
                    {
                        signal_handler_post_reload,
                        #{
                            reason => Reason,
                            stacktrace => ?get_stacktrace(Stacktrace),
                            signal => Signal
                        }
                    }
                }
        end,
    _ =
        case Result of
            {error, Info} ->
                async_stop(CallName, Info);
            _ ->
                ok
        end,
    {ok, S};

handle_event(_, S) ->
    {ok, S}.


handle_call(_, S) ->
    {ok, S}.

%% -----------------------------------------------------------------------------
%% Exported Internals:

default_pre_reload() ->
    ok.


default_post_reload(_) ->
    ok.

%% -----------------------------------------------------------------------------
%% Internals:

options(#{notify_tag := Value}=Opts, S) ->
    options(
        maps:remove(notify_tag, Opts),
        S#?S{notify_tag = Value}
    );

options(#{delete_on_terminate := Value}=Opts, S) ->
    options(
        maps:remove(delete_on_terminate, Opts),
        S#?S{
            delete_on_terminate =
                if
                    erlang:is_boolean(Value) ->
                        Value;
                    true ->
                        ?DEFAULT_DELETE_ON_TERMINATE
                end
        }
    );

options(#{change_priority := Value}=Opts, S) ->
    options(
        maps:remove(change_priority, Opts),
        S#?S{
            change_priority =
                if
                    erlang:is_boolean(Value) ->
                        Value;
                    true ->
                        ?DEFAULT_CHANGE_PRIORITY
                end
        }
    );

options(#{notify_method := Value}=Opts, S) ->
    options(
        maps:remove(notify_method, Opts),
        S#?S{
            notify_method =
                if
                    Value == call orelse Value == cast ->
                        Value;
                    true ->
                        ?DEFAULT_NOTIFY_METHOD
                end
        }
    );

options(#{reload_on_signal := Value}=Opts, S) ->
    options(
        maps:remove(reload_on_signal, Opts),
        S#?S{
            reload_on_signal =
            if
                Value == true ->
                    sighup;
                Value == false ->
                    false;
                erlang:is_atom(Value) -> % Signal name
                    Value;
                true ->
                    ?DEFAULT_RELOAD_ON_SIGNAL
            end
        }
    );

options(#{signal_handler_pre_reload := Value}=Opts, S) ->
    options(
        maps:remove(signal_handler_pre_reload, Opts),
        S#?S{
            signal_handler_pre_reload =
            if
                erlang:is_function(Value, 0) ->
                    Value;
                true ->
                    ?DEFAULT_SIGNAL_HANDLER_PRE_RELOAD
            end
        }
    );

options(#{signal_handler_post_reload := Value}=Opts, S) ->
    options(
        maps:remove(signal_handler_post_reload, Opts),
        S#?S{
            signal_handler_post_reload =
            if
                erlang:is_function(Value, 1) ->
                    Value;
                true ->
                    ?DEFAULT_SIGNAL_HANDLER_POST_RELOAD
            end
        }
    );

options(#{signal_handler_reload_sync := Value}=Opts, S) ->
    options(
        maps:remove(signal_handler_reload_sync, Opts),
        S#?S{
            signal_handler_reload_sync =
            if
                erlang:is_boolean(Value) ->
                    Value;
                true ->
                    ?DEFAULT_SIGNAL_HANDLER_RELOAD_SYNC
            end
        }
    );

options(#{reload_on_timer := Value}=Opts, S) ->
    options(
        maps:remove(reload_on_timer, Opts),
        S#?S{
            reload_on_timer =
            if
                Value == true ->
                    timer:minutes(1);
                erlang:is_integer(Value) andalso Value > 0 ->
                    timer:minutes(Value);
                true ->
                    ?DEFAULT_RELOAD_ON_TIMER
            end
        }
    );

options(#{timer_pre_reload := Value}=Opts, S) ->
    options(
        maps:remove(timer_pre_reload, Opts),
        S#?S{
            timer_pre_reload =
            if
                erlang:is_function(Value, 0) ->
                    Value;
                true ->
                    ?DEFAULT_TIMER_PRE_RELOAD
            end
        }
    );

options(#{timer_post_reload := Value}=Opts, S) ->
    options(
        maps:remove(timer_post_reload, Opts),
        S#?S{
            timer_post_reload =
            if
                erlang:is_function(Value, 1) ->
                    Value;
                true ->
                    ?DEFAULT_TIMER_POST_RELOAD
            end
        }
    );

options(#{subscribers := Value}=Opts, S) ->
    options(
        maps:remove(subscribers, Opts),
        S#?S{
            subscribers =
            if
                erlang:is_map(Value) ->
                    Value;
                true ->
                    #{}
            end
        }
    );

options(_, S) ->
    maybe_setup_timer(maybe_setup_signal_handler(S)).


load(S) ->
    case read_and_filter(S) of
        {ok, S2} ->
            keep(S2);
        Err ->
            Err
    end.


read_and_filter(#?S{readers = Readers, filters = Filters}=S) ->
    case cfg:read(Readers) of
        {ok, _, Cfg} ->
            case cfg:filter(Cfg, Filters) of
                {ok, Cfg2} ->
                    {ok, S#?S{config = Cfg, filtered_config = Cfg2}};
                Err ->
                    Err
            end;
        Err ->
            Err
    end.


keep(#?S{keeper = Keeper, filtered_config = Cfg}=S) ->
    case cfg:init(Keeper) of
        ok ->
            case cfg:set(Keeper, Cfg) of
                ok ->
                    {ok, S};
                Err ->
                    Err
            end;
        Err ->
            Err
    end.


do_reload(#?S{config = Cfg}=S) ->
    _ =
        if
            S#?S.change_priority ->
                erlang:process_flag(priority, high);
            true ->
                ok
        end,
    Result =
        case load(S) of
            {ok, #?S{config = Cfg2}=S2} ->
                _ =
                    if
                        Cfg /= Cfg2 ->
                            _ = notify_subscribers(
                                maps:to_list(S#?S.subscribers),
                                Cfg,
                                Cfg2,
                                S#?S.notify_method,
                                S#?S.notify_tag
                            );
                        true ->
                            ok
                    end,
                {ok, S2};
            Err ->
                Err
        end,
    _ =
        if
            S#?S.change_priority ->
                erlang:process_flag(priority, normal);
            true ->
                ok
        end,
    Result.


notify_subscribers(
    [{Subscriber, Filters} | Subscribers],
    OldCfg,
    NewCfg,
    NotifyMethod,
    Tag
) ->
    _ = notify_subscriber(
        Filters,
        Subscriber,
        OldCfg,
        NewCfg,
        NotifyMethod,
        Tag
    ),
    notify_subscribers(Subscribers, OldCfg, NewCfg, NotifyMethod, Tag);

notify_subscribers(_, _, _, _, _) ->
    ok.


notify_subscriber(
    Filters,
    Subscriber,
    OldCfg,
    NewCfg,
    NotifyMethod,
    Tag
) ->
    case {cfg:filter(OldCfg, Filters), cfg:filter(NewCfg, Filters)} of
        {{ok, X}, {ok, X}} ->
            ok;
        {{ok, OldValue}, {ok, NewValue}} ->
            notify_subscriber(
                Subscriber,
                {{value, OldValue}, {value, NewValue}},
                NotifyMethod,
                Tag
            );
        {_, {ok, NewValue}} -> % {assume not found, _}
            notify_subscriber(
                Subscriber,
                {undefined, {value, NewValue}},
                NotifyMethod,
                Tag
            );
        {{ok, OldValue}, _} -> % {_, assume not found}
            notify_subscriber(
                Subscriber,
                {{value, OldValue}, undefined},
                NotifyMethod,
                Tag
            );
        _ -> % {assume not found, assume not found}
            ok
    end.


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
            gen_server:call(Subscriber, {Tag, Msg}, 1000)
        catch
            _:_ ->
                ok
        end,
    ok.


do_subscribe(#?S{subscribers = Subscribers}=S, Subscriber, Filters) ->
    S#?S{subscribers = Subscribers#{Subscriber => Filters}}.


do_unsubscribe(#?S{subscribers = Subscribers}=S, Subscriber) ->
    S#?S{subscribers = maps:without([Subscriber], Subscribers)}.


maybe_setup_signal_handler(
    #?S{signal_handler_state = undefined, reload_on_signal = false}=S
) ->
    S;

maybe_setup_signal_handler(
    #?S{
        signal_handler_state = SHS, % Signal Handler State
        reload_on_signal = false
    }= S
) ->
    _ =
        try
            gen_event:delete_handler(erl_signal_server, {?MODULE, SHS}, SHS)
        catch
            _:_ ->
                ok
        end,
    S#?S{signal_handler_state = undefined};

maybe_setup_signal_handler(
    #?S{
        reload_on_signal = Signal,
        signal_handler_pre_reload = PreReload,
        signal_handler_post_reload = PostReload,
        signal_handler_reload_sync = Sync,
        register_name = RegisterName
    }=S
) ->
    SHS = #?SHS{
        signal = Signal,
        register_name = RegisterName,
        pre_reload_function = PreReload,
        post_reload_function = PostReload,
        sync_reload = Sync
    },
    _ = add_signal_handler(
        try
            gen_event:which_handlers(erl_signal_server)
        catch
            _:_ ->
                []
        end,
        SHS
    ),
    S#?S{signal_handler_state = SHS}.


%% Handler exists:
add_signal_handler(
    [{?MODULE, Id} | _],
    SHS
) when SHS == Id ->
    ok;
%% I did register myself for other signal or pre-reload or post-reload funs:
add_signal_handler(
    [{?MODULE, #?SHS{register_name = RegisterName}=OldSHS}=Handler | _],
    #?SHS{register_name = RegisterName, signal = Signal}=SHS
) -> % RegisterName == RegisterName
    _ =
        try
            ok = gen_event:swap_handler(
                erl_signal_server,
                {Handler, OldSHS},
                {{?MODULE, SHS}, SHS}
            ),
            os:set_signal(Signal, handle)
        catch
            _:_ ->
                ok
        end,
    ok;

add_signal_handler([], #?SHS{signal = Signal}=SHS) ->
    _ =
        try
            _ = gen_event:add_handler(
                erl_signal_server,
                {?MODULE, SHS},
                SHS
            ),
            os:set_signal(Signal, handle)
        catch
            _:_ ->
                ok
        end,
    ok;

add_signal_handler([_ | Handlers], SHS) ->
    add_signal_handler(Handlers, SHS).


maybe_setup_timer(
    #?S{timer_state = undefined, reload_on_timer = false}=S
) ->
    S;

maybe_setup_timer(
    #?S{
        timer_state = TimerRef,
        reload_on_timer = false
    }=S
) ->
    _ = erlang:cancel_timer(TimerRef),
    S#?S{timer_state = undefined};

maybe_setup_timer(#?S{timer_state = TS, reload_on_timer = Time}=S) ->
    _ =
        if
            TS /= undefined ->
                erlang:cancel_timer(TS);
            true ->
                ok
        end,
    S#?S{
        timer_state = erlang:send_after(
            Time,
            erlang:self(),
            ?TIMEOUT_TAG,
            []
        )
    }.


async_stop(Server, Info) ->
    gen_server:cast(Server, {?STOP_TAG, Info}).
