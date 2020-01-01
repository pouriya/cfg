%%% ----------------------------------------------------------------------------
%%% @author <pouriya.jahanbakhsh@gmail.com>
%%% @hidden

%% -----------------------------------------------------------------------------
-module(cfg).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

-export([load/3, read/1, filter/2, keep/2, get/2, get/3, set/3, delete/2, delete/1, check_filters/1]).


load(Readers, Filters, Keeper) ->
    
    case read(Readers) of
        {ok, Cfg} ->
            case filter(Cfg, Filters) of
                {ok, Cfg2, Unknown} ->
                    case keep(Cfg2, Keeper) of
                        ok ->
                            {ok, Cfg2, Unknown};
                        {_, {Reason, ErrParams}} ->
                            {
                                error,
                                {
                                    Reason,
                                    ErrParams#{
                                        readers => Readers,
                                        filters => Filters
                                    }
                                }
                            }
                    end;
                {_, {Reason, ErrParams}} ->
                    {error, {Reason, ErrParams#{readers => Readers}}}
            end;
        Err ->
            Err
    end.


read(Readers) ->
    cfg_reader:do(Readers).


filter(Cfg, Filters) ->
    cfg_filter:do(Filters, Cfg).


check_filters(Filters) ->
    cfg_filter:check(Filters).


keep(Cfg, Keeper) ->
    cfg_keeper:keep(Keeper, Cfg).


get(Keeper, Keys) ->
    cfg_keeper:get(Keeper, Keys).


get(Keeper, Keys, Default) ->
    cfg_keeper:get(Keeper, Keys, Default).


set(Keeper, Keys, Value) ->
    cfg_keeper:set(Keeper, Keys, Value).


delete(Keeper, Keys) ->
    cfg_keeper:delete(Keeper, Keys).


delete(Keeper) ->
    cfg_keeper:delete(Keeper).
