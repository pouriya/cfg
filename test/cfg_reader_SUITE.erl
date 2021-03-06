%% Auto-generated by https://github.com/Pouriya-Jahanbakhsh/estuff
%% -----------------------------------------------------------------------------
-module(cfg_reader_SUITE).
-author('pouriya.jahanbakhsh@gmail.com').
%% -----------------------------------------------------------------------------
%% Exports:

%% ct callbacks:
-export([init_per_suite/1
        ,end_per_suite/1
        ,all/0
        ,init_per_testcase/2
        ,end_per_testcase/2]).

%% Testcases:
-export(['1'/1
        ,'2'/1
        ,'3'/1
        ,'4'/1
        ,'5'/1
        ,'6'/1
        ,'7'/1
        ,'8'/1
        ,'9'/1
        ,'10'/1]).

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------------
%% ct callbacks:


all() ->
    ToInteger =
        fun
            ({Func, 1}) ->
                try
                    erlang:list_to_integer(erlang:atom_to_list(Func))
                catch
                    _:_ ->
                        0
                end;
            (_) -> % Arity > 1 | Arity == 0
                0
        end,
    % contains 0 for other functions:
    Ints = [ToInteger(X) || X <- ?MODULE:module_info(exports)],
    % 1, 2, ...
    PosInts = lists:sort([Int || Int <- Ints, Int > 0]),
    % '1', '2', ...
    [erlang:list_to_atom(erlang:integer_to_list(X)) || X <- PosInts].


init_per_suite(Cfg) ->
    application:start(sasl),
    Cfg.


end_per_suite(Cfg) ->
    application:stop(sasl),
    Cfg.


init_per_testcase(_TestCase, Cfg) ->
    Cfg.


end_per_testcase(_TestCase, _Cfg) ->
    ok.

%% -----------------------------------------------------------------------------
%% Test cases:

'1'(_) ->
    ?assertMatch([], cfg_reader:normalize(#{})),
    ?assertMatch([1, 3.14, <<"foo">>, [{key, [[{bar, baz}]]}], atom], cfg_reader:normalize([1, 3.14, "foo", #{key => [#{bar => baz}]}, atom])),

    ?assertMatch(ok, cfg:is_cfg([])),
    ?assertMatch(ok, cfg:is_cfg([{k, v}])),
    ?assertMatch(ok, cfg:is_cfg([{k, v}, {k2, v2}])),
    ?assertMatch(
        ok,
        cfg:is_cfg(
            [
                {k, v},
                {k3, [{k4, [1, 3.14, <<"foo">>, [bar, baz]]}]},
                {k2, v2}
            ]
        )
    ),

    ?assertMatch(
        {error, {check_config, #{allowed_type := proplist}}},
        cfg:is_cfg(bad_cfg)
    ),

    ?assertMatch(
        {error, {check_config, #{allowed_type := proplist}}},
        cfg:is_cfg([{k, v}, bad_cfg])
    ),

    ?assertMatch(
        {
            error,
            {
                check_config,
                #{
                    allowed_type := atom,
                    key := <<"key">>,
                    parent_keys := [k],
                    reason := bad_key
                }
            }
        },
        cfg:is_cfg([{k, [{<<"key">>, value}]}])
    ),

    ?assertMatch(
        {
            error,
            {
                check_config,
                #{
                    allowed_types := [proplist,list,atom,binary,number],
                    key := key,
                    list_element := {this_is,bad,list_element},
                    parent_keys := [k]
                }
            }
        },
        cfg:is_cfg([{k, [{key, [{this_is, bad, list_element}]}]}])
    ),

    ?assertMatch(
        {
            error,
            {
                check_config,
                #{
                    allowed_type := list,
                    key := key,
                    list := list,
                    parent_keys := [k]
                }
            }
        },
        cfg:is_cfg([{k, [{key, [bad|list]}]}])
    ),
    ok.

'2'(_) ->
    _ = cfg_test_utils:clean_env(),
    Cfg = [
        {k5, [{k6, [1, {k7, bar}, 3.14, [<<"baz">>]]}]}
    ],
    _ = cfg_test_utils:set_env(Cfg),

    ?assertMatch(
        {
            ok,
            [
                {
                    env,
                    cfg,
                    [{k5, [{k6, [1, {k7, bar}, 3.14, [<<"baz">>]]}]}]
                }
            ],
            [
                {
                    k5,
                    [
                        {
                            k6,
                            [1, {k7, bar, [{env, cfg}]}, 3.14, [<<"baz">>]],
                            [{env, cfg}]
                        }
                    ],
                    [{env, cfg}]
                }
            ]
        },
        cfg:read([{env, cfg}])
    ),

    _ = cfg_test_utils:clean_env(),
    Cfg2 = [{k, [{k2, v2}]}],
    _ = cfg_test_utils:set_env(Cfg2),

    ?assertMatch(
        {
            ok,
            [
                {env,cfg,[{k,[{k2,v2}]}]},
                {env,cfg,[{k,[{k2,v2}]}]},
                {env,cfg,[{k,[{k2,v2}]}]}
            ],
            [{k,[{k2,v2,[{env,cfg}]}],[{env,cfg}]}]},
        cfg:read([{env, cfg}, {env, cfg}, {env, cfg}])
    ),

    ?assertMatch(
        {
            ok,
            [{env, {cfg, k}, [{k2, v2}]}],
            [{k2, v2, [{env, {cfg, k}}]}]
        },
        cfg:read([{env, {cfg, k}}])
    ),


    _ = cfg_test_utils:clean_env(),
    Cfg3 = [
        {foo, [{k2, v2}, {k3, v3}, {k4, [{k5, <<"foo">>}]}]},
        {bar, [{k2, v3}, {k3, v3}, {k4, [{k5, <<"bar">>}]}]}
    ],
    _ = cfg_test_utils:set_env(Cfg3),
    ?assertMatch(
        {
            ok,
            [
                {env, {cfg, foo}, [{k2, v2}, {k3, v3}, {k4, [{k5, <<"foo">>}]}]},
                {env, {cfg, bar}, [{k2, v3}, {k3, v3}, {k4, [{k5, <<"bar">>}]}]}
            ],
            [
                {k2, v3, [{env, {cfg, bar}}]},
                {k3, v3, [{env, {cfg, bar}}]},
                {
                    k4,
                    [{k5, <<"bar">>, [{env, {cfg, bar}}]}],
                    [{env, {cfg, bar}}, {env, {cfg, foo}}]
                }
            ]
        },
        cfg:read([{env, {cfg, foo}}, {env, {cfg, bar}}])
    ),
    ?assertMatch({error, _}, cfg:read([bad_reader])),


    _ = cfg_test_utils:clean_env(),
    _ = application:set_env(cfg, k, erlang:self()),
    ?assertMatch({error, {_, #{previous_error :=  {check_config, _}}}}, cfg:read([{env, cfg}])),

    ?assertMatch({error, {_, #{k := v}}}, cfg:read([{test, {error, #{k => v}}}])),
    ?assertMatch({error, {_, #{returned_value :=  unknown_ret}}}, cfg:read([{test, unknown_ret}])),
    ?assertMatch({error, {_, #{exception :=  oops}}}, cfg:read([{test, {exception, oops}}])),

    ok.



'3'(Cfg) ->
    _ = Cfg,
    % Complete coverage:
    _ = cfg_reader:module(env),
    _ = cfg_reader:module(erl),
    _ = cfg_reader:module(shell),
    _ = cfg_reader:module(x),
    ok.



'4'(Cfg) ->
    _ = Cfg,
    ok.



'5'(Cfg) ->
    _ = Cfg,
    ok.



'6'(Cfg) ->
    _ = Cfg,
    ok.



'7'(Cfg) ->
    _ = Cfg,
    ok.



'8'(Cfg) ->
    _ = Cfg,
    ok.



'9'(Cfg) ->
    _ = Cfg,
    ok.



'10'(Cfg) ->
    _ = Cfg,
    ok.
