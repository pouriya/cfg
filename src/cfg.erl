%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% API
-export(
    [
        load/3,
        read_and_filter/2
    ]
).

%% Reader API:
-export(
    [
        read/1,
        is_cfg/1
    ]
).

%% Filter API:
-export(
    [
        filter/2,
        get_application_filters/1,
        get_module_filters/1
    ]
).

%% Keeper API:
-export(
    [
        init/1,
        set/2,
        set/3,
        get/1,
        get/2,
        get/3,
        delete/2,
        delete/1
    ]
).

%% Server API:
-export(
    [
        start_link/5,
        reload/1,
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
    ]
).

%% -----------------------------------------------------------------------------
%% API:

load(Readers, Filters, Keeper) ->
    case read_and_filter(Readers, Filters) of
        {ok, Cfg}=Ok ->
            case init(Keeper) of
                ok ->
                    case set(Keeper, Cfg) of
                        ok ->
                            Ok;
                        {_, {Reason, ErrParams}} ->
                            {
                                error,
                                {
                                    Reason,
                                    ErrParams#{
                                        readers => Readers,
                                        filters => Filters,
                                        config => Cfg
                                    }
                                }
                            }
                    end;
                {_, {Reason, ErrParams}} ->
                    {
                        error,
                        {
                            Reason,
                            ErrParams#{
                                readers => Readers,
                                filters => Filters,
                                config => Cfg
                            }
                        }
                    }
            end;
        {_, {Reason, ErrParams}} ->
            {
                error,
                {
                    Reason,
                    ErrParams#{
                        readers => Readers,
                        filters => Filters,
                        keeper => Keeper
                    }
                }
            }
    end.


read_and_filter(Readers, Filters) ->
    case read(Readers) of
        {ok, _, Cfg} ->
            filter(Cfg, Filters);
        {_, {Reason, ErrParams}} ->
            {
                error,
                {Reason, ErrParams#{readers => Readers, filters => Filters}}
            }
    end.

%% -----------------------------------------------------------------------------
%% Reader API:

read(Readers) ->
    cfg_reader:do(Readers).


is_cfg(Cfg) ->
    cfg_reader:is_cfg(Cfg).

%% -----------------------------------------------------------------------------
%% Filter API:

filter(Cfg, Filters) ->
    cfg_filter:do(Filters, Cfg).


get_application_filters(AppName) ->
    cfg_filter:get_application_filters(AppName).


get_module_filters(Mod) ->
    cfg_filter:get_module_filters(Mod).

%% -----------------------------------------------------------------------------
%% Keeper API:

init(Keeper) ->
    cfg_keeper:init(Keeper).


set(Keeper, Cfg) ->
    cfg_keeper:set(Keeper, Cfg).


set(Keeper, Keys, Value) ->
    cfg_keeper:set(Keeper, Keys, Value).


get(Keeper) ->
    cfg_keeper:get(Keeper).


get(Keeper, Keys) ->
    cfg_keeper:get(Keeper, Keys).


get(Keeper, Keys, Default) ->
    cfg_keeper:get(Keeper, Keys, Default).


delete(Keeper, Keys) ->
    cfg_keeper:delete(Keeper, Keys).


delete(Keeper) ->
    cfg_keeper:delete(Keeper).

%% -----------------------------------------------------------------------------
%% Server API:

start_link(RegisterName, Readers, Filters, Keeper, Opts) ->
    cfg_server:start_link(RegisterName, Readers, Filters, Keeper, Opts).


reload(Server) ->
    cfg_server:reload(Server).


subscribe(Server, Keys) ->
    cfg_server:subscribe(Server, Keys).


unsubscribe(Server) ->
    cfg_server:unsubscribe(Server).


subscribe(Server, Filters, Subscriber) ->
    cfg_server:subscribe(Server, Filters, Subscriber).


unsubscribe(Server, Subscriber) ->
    cfg_server:unsubscribe(Server, Subscriber).


change_options(Server, Opts) ->
    cfg_server:change_options(Server, Opts).


set_readers(Proc, Readers) ->
    cfg_server:set_readers(Proc, Readers).


set_filters(Proc, Filters) ->
    cfg_server:set_filters(Proc, Filters).


set_keeper(Proc, Keeper) ->
    cfg_server:set_keeper(Proc, Keeper).


info(Proc) ->
    cfg_server:info(Proc).


stop(Server) ->
    cfg_server:stop(Server).
