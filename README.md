# Simple and modern configuration management in Erlang

## Features
* Read config from different places:  
```sh
~/cfg $ export FOO_KEY=VALUE
```
```erlang
1> application:set_env(bar, key2, value2). 
ok

2> cfg:read([{shell, "FOO"}, {env, bar}]).
{
    ok,
    % Config from different places:
    [
        {shell, "FOO", [{key, <<"VALUE">>}]},
        {env, bar, [{key2,value2}]}
    ],
    % The whole config
    [
        {key2, value2, [{env, bar}]},
        {key, <<"VALUE">>, [{shell, "FOO"}]}
    ]
}
```
* Override configuration from different places:  
```sh
~/cfg $ echo {server, []}. > my.config
~/cfg $ export MY_SERVER__HOST=127.0.0.1
~/cfg $ export MY_SERVER__PORT=8080
```
```erlang
1> application:set_env(my, server, [{port, 9090}]).
ok

2> cfg:read([{shell, "MY"}, {erl, "my.config"}, {env, my}]).
{
    ok,
    [
        {shell, "MY", [{server, [{host, <<"127.0.0.1">>}, {port, 8080}]}]},
        {erl, "my.config", [{server,[]}]},
        {env, my, [{server, [{port, 9090}]}]}
    ],
    [
        {
            server,
            [
                {port, 9090, [{env, my}]}, % This key comes from app env of 'my' 
                {host, <<"127.0.0.1">>, [{shell, "MY"}]} % This key comes from shell
            ],
            [{env, my}, {erl, "my.config"}, {shell, "MY"}] % 'server' key comes from three places
        }
    ]
}
```
* Filter configuration parameters:  
```sh
~/cfg $ export BAR_KEY=value
```  
```erlang
1> cfg:read_and_filter([{shell, "BAR"}], []). % no filter
{ok, []}

2> cfg:read_and_filter([{shell, "BAR"}], [{key, atom}]). % I want it be atom 
{ok, [{key, value}]}

3> cfg:read_and_filter([{shell, "BAR"}], [{key, binary}]). % I want it be binary
{
    error,
    {
        filter_config,
        #{
            allowed_type => binary,
            filter => {key, binary},
            filters => [{key, binary}],
            index => 1,
            key => key,
            key_filter => binary,
            readers => [{shell, "BAR"}],
            value => value
        }
    }
}

4> cfg:read_and_filter([{shell, "BAR"}], [{key, try_binary}]). % Tries to convert its value to binary
{ok, [{key, <<"value">>}]}

5> cfg:read_and_filter([{shell, "BAR"}], [{key, try_binary}, {undefined_key, try_integer}]). % 'undefined_key' does not exists in config
{
    error,
    {
        filter_config,
        #{
            filter => {undefined_key, try_integer},
            filters => [{key, try_binary}, {undefined_key, try_integer}],
            index => 2,
            key => undefined_key,
            key_filter => try_integer,
            reason => value_not_found
        }
    }
}

% Define default value as 3rd element of a filter tuple:
6> cfg:read_and_filter([{shell, "BAR"}], [{key, try_binary}, {undefined_key, try_integer, <<"1995">>}]).
{ok, [{key, <<"value">>}, {undefined_key, 1995}]}
```  
**cfg** supports many ready-to-use filters and you can five your own filters to it. Also **cfg** can infer your wanted type from your default value!  

* Keep configuration:  
```erlang
1> Keeper = {ets, table_name}.
{ets,table_name}

2> cfg:init(Keeper).
ok

3> cfg:set(Keeper, [{k, v}, {k2, v2}]).
ok

4> ets:tab2list(table_name).
[{k, v}, {k2, v2}]

5> cfg:get(Keeper, k).
{ok, v}

6> cfg:get(Keeper, k3).
not_found

7> cfg:get(Keeper, k3, default_value).
{ok, default_value}
```
You can use above features in one function call:  
```erlang
1> cfg:load([{shell, "BAR"}], [{key, try_binary}, {undefined_key, try_integer, <<"1995">>}], {ets, tab_name}).
{ok, [{key, <<"value">>}, {undefined_key, 1995}]}

2> ets:tab2list(tab_name).
[{key, <<"value">>}, {undefined_key, 1995}]
```

* **cfg** comes with a ready-to-use server process which supports reloading config on OS signals, reloading on timer and subscription to know changes in config during reload!  
```erlang
1> cfg:start_link(
    {local, server}, % Server name
    [{shell, "BAR"}], % Reader(s)
    [{key, try_binary}, {undefined_key, try_integer, <<"1995">>}], % Filter(s)
    {ets, tab_name}, % Keeper
    #{ % Server options
        reload_on_signal => sighup,
        signal_handler_post_reload => fun(ok) -> io:format("Config reloaded!\n") end
    }
).
{ok,<0.114.0>}

2> os:getpid().
"29867"

3> os:cmd("kill -1 29867"), ok
ok
Config reloaded!

4> cfg:subscribe(server, [{key, try_binary}]). % I want to know changes just for key
ok

5> os:cmd("kill -1 29867"), ok
ok
Config reloaded!

6> flush().
ok % value of 'key' did not change


7> os:putenv("BAR_KEY", "value2").            
true

8> os:cmd("kill -1 29867"), ok
ok
Config reloaded!

9> flush().
Shell got {
    config,
    {
        {value, [{key, <<"value">>}]}, % Old value 
        {value,[{key, <<"value2">>}]}  % New value
    }
}
ok
```

## `Author`
`pouriya.jahanbakhsh@gmail.com`

## `Hex version`
[**17.12.26**](https://hex.pm/packages/cfg)
