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
    signal_handler_pre_reload/2,
    signal_handler_post_reload/3
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
        signal_handler_state
    }
).

-define(O, options).
-record(?O, {
    error_unknown_config,
    notify_method,
    notify_tag,
    change_priority,
    delete_on_terminate,
    reload_on_signal,
    signal_handler_pre_reload,
    signal_handler_post_reload,
    signal_handler_reload_sync
}).

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

-include("cfg_stacktrace.hrl").

%% -----------------------------------------------------------------------------
%% API:

start_link(RegisterName, Readers, Filters, Keeper, Opts) ->
    case cfg_filter:check(Filters) of
        ok ->
            gen_server:start_link(
                RegisterName,
                ?MODULE,
                {RegisterName, Readers, Filters, Keeper, Opts},
                []
            );
        Err ->
            Err
    end.


reload(Proc) ->
    gen_server:call(Proc, ?RELOAD_TAG, infinity).


async_reload(Proc) ->
    gen_server:cast(Proc, ?RELOAD_TAG).


subscribe(Proc, Keys) ->
    subscribe(Proc, Keys, erlang:self()).


subscribe(Proc, Filters, Subscriber) ->
    case cfg_filter:check(Filters) of
        ok ->
            gen_server:call(Proc, {?SUBSCRIBE_TAG, Subscriber, Filters});
        Err ->
            Err
    end.


unsubscribe(Proc) ->
    unsubscribe(Proc, erlang:self()).


unsubscribe(Proc, Subscriber) ->
    gen_server:call(Proc, {?UNSUBSCRIBE_TAG, Subscriber}).


change_options(Proc, Opts) when erlang:is_map(Opts) ->
    gen_server:call(Proc, {?CHANGE_OPTIONS_TAG, Opts}).


set_readers(Proc, Readers) ->
    gen_server:call(Proc, {?SET_READERS_TAG, Readers}).


set_filters(Proc, Filters) ->
    case cfg_filter:check(Filters) of
        ok ->
            gen_server:call(Proc, {?SET_FILTERS_TAG, Filters});
        Err ->
            Err
    end.


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
    S = #?S{
        readers = Readers,
        filters = Filters,
        keeper = Keeper,
        subscribers = maps:get(subscribers, Opts, #{}),
        options = options(
            Opts,
            #?O{
                error_unknown_config = false,
                notify_method = message,
                notify_tag = config,
                change_priority = false,
                delete_on_terminate = true,
                reload_on_signal = false,
                signal_handler_pre_reload =
                    fun ?MODULE:signal_handler_pre_reload/2,
                signal_handler_post_reload =
                    fun ?MODULE:signal_handler_post_reload/3
            }
        ),
        register_name = RegisterName,
        signal_handler_state = undefined
    },
    case load(S) of
        {ok, S2} ->
            {ok, maybe_setup_signal_handler(S2)};
        {_, Info} ->
            {stop, Info}
    end.


%% 'gen_server':
handle_info(_, #?S{}=S) ->
    {noreply, S};
%% 'gen_event':
handle_info(_, S) ->
    {ok, S}.

%% 'gen_server':
terminate(_, #?S{}=S) ->
    _ =
        if
            (S#?S.options)#?O.delete_on_terminate ->
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
    {
        reply,
        ok,
        maybe_setup_signal_handler(S#?S{options = options(Opts, S#?S.options)})
    };

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
    try PreReload(RegisterName, Signal) of
        ok ->
            CallName =
                case RegisterName of
                    {local, LocalName} ->
                        LocalName;
                    _ ->
                        RegisterName
                end,
            Result =
                try
                    if
                        SyncReload ->
                            reload(CallName);
                        true ->
                            async_reload(CallName)
                    end
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
            _ =
                try
                    PostReload(RegisterName, Signal, Result)
                catch
                    _:_ ->
                        ok
                end,
            {ok, S}
    catch
        _:_ ->
            {ok, S}
    end;

handle_event(_, S) ->
    {ok, S}.


handle_call(_, S) ->
    {ok, S}.

%% -----------------------------------------------------------------------------
%% Exported Internals:

signal_handler_pre_reload(_, _) ->
    ok.


signal_handler_post_reload(_, _, _) ->
    ok.

%% -----------------------------------------------------------------------------
%% Internals:

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
            notify_method =
                if
                    Value == call orelse Value == cast ->
                        Value;
                    true ->
                        message
                end
        }
    );

options(#{reload_on_signal := Value}=Opts, Rec) ->
    options(
        maps:remove(reload_on_signal, Opts),
        Rec#?O{
            reload_on_signal =
            if
                Value == true ->
                    sighup;
                Value == false ->
                    false;
                erlang:is_atom(Value) -> % Signal name
                    Value;
                true ->
                    false
            end
        }
    );

options(#{signal_handler_pre_reload := Value}=Opts, Rec) ->
    options(
        maps:remove(signal_handler_pre_reload, Opts),
        Rec#?O{
            signal_handler_pre_reload =
            if
                erlang:is_function(Value, 2) ->
                    Value;
                true ->
                    fun ?MODULE:signal_handler_pre_reload/2
            end
        }
    );

options(#{signal_handler_post_reload := Value}=Opts, Rec) ->
    options(
        maps:remove(signal_handler_post_reload, Opts),
        Rec#?O{
            signal_handler_post_reload =
            if
                erlang:is_function(Value, 3) ->
                    Value;
                true ->
                    fun ?MODULE:signal_handler_post_reload/3
            end
        }
    );

options(#{signal_handler_reload_sync := Value}=Opts, Rec) ->
    options(
        maps:remove(signal_handler_reload_sync, Opts),
        Rec#?O{
            signal_handler_reload_sync =
            if
                erlang:is_boolean(Value) ->
                    Value;
                true ->
                    true
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
    case cfg:read(Readers) of
        {ok, Cfg} ->
            case cfg:filter(Cfg, Filters) of
                {ok, Cfg2, Unknown} ->
                    if
                        Opts#?O.error_unknown_config ->
                            {error, {unknown_config, #{values => Unknown}}};
                        true ->
                            {ok, S#?S{config = Cfg, filtered_config = Cfg2}}
                    end;
                Err ->
                    Err
            end;
        Err ->
            Err
    end.


keep(#?S{keeper = Keeper, filtered_config = Cfg}=S) ->
    case cfg:init(Keeper) of
        ok ->
            case cfg:load(Cfg, Keeper) of
                ok ->
                    {ok, S};
                Err ->
                    Err
            end;
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
                    (S#?S.options)#?O.notify_method,
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

notify_subscribers(_, _, _, _, _) ->
    ok.


notify_subscriber(
    Filters,
    Subscriber,
    OldCfg,
    NewCfg,
    Notifier,
    Tag
) ->
    case {cfg:filter(OldCfg, Filters), cfg:filter(NewCfg, Filters)} of
        {{ok, X, _}, {ok, X, _}} ->
            ok;
        {{ok, OldValue, _}, {ok, NewValue, _}} ->
            notify_subscriber(
                Subscriber,
                {{value, OldValue}, {value, NewValue}},
                Notifier,
                Tag
            );
        {_, {ok, NewValue, _}} ->
            notify_subscriber(
                Subscriber,
                {undefined, {value, NewValue}},
                Notifier,
                Tag
            );
        {{ok, OldValue, _}, _} ->
            notify_subscriber(
                Subscriber,
                {{value, OldValue}, undefined},
                Notifier,
                Tag
            );
        _ ->
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
            gen_server:call(Subscriber, {Tag, Msg}, infinity)
        catch
            _:_ ->
                ok
        end,
    ok.


do_subscribe(#?S{subscribers = Subscribers}=S, Subscriber, Filters) ->
    S#?S{subscribers = Subscribers#{Subscriber => Filters}}.


do_unsubscribe(#?S{subscribers = Subscribers}=S, Subscriber) ->
    case maps:take(Subscriber, Subscribers) of
        {_, Subscribers2} ->
            S#?S{subscribers = Subscribers2};
        _ ->
            S
    end.


maybe_setup_signal_handler(
    #?S{
        signal_handler_state = undefined,
        options = #?O{reload_on_signal = false}
    }= S
) ->
    S;

maybe_setup_signal_handler(
    #?S{
        signal_handler_state = SHS, % Signal Handler State
        options = #?O{reload_on_signal = false}
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
        options = #?O{
            reload_on_signal = Signal,
            signal_handler_pre_reload = PreReload,
            signal_handler_post_reload = PostReload,
            signal_handler_reload_sync = Sync
        },
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
