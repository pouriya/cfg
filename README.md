# Simple and modern config parser
**cfg** is a config parser for parsing `.cfg` files and converting cfg file data to different Erlang terms.

## Features
* Parsing file and returning its data as proplist.
* Parsing file and inserting its data in an ETS table.  
* Parsing file and setting its data as environment variables of an application.  
* Parsing file and calls a callback for every value which it founds in file.
* Providing clean and human-readable reason for errors.

## Example
I want to parse following config file named `test.cfg`.  
```cfg
# This is a comment


# in Erlang blow line will convert to {foo, bar}
foo = bar


# in Erlang blow data will convert to:
# {list, [true
#        ,false
#        ,0
#        ,3.14
#        ,-1
#        ,atom
#        ,other_atom_because_of_underline
#        ,atom@with@at@sing
#        ,<<"binary_because_of_qoutation">>
#        ,<<"binary because of space">>
#        ,<<"Binary_because_of_B_at_the_first">>
#        ,<<"@binary_because_of_@_at_the_first">>
#        ,<<"_binary_because_of_underline_at_the_first">>]}
> list
true
false
0
3.14
-1
atom
other_atom_because_of_underline
atom@with@at@sing
"binary_because_of_qoutation"
Binary_because_of_B_at_the_first
@binary_because_of_@_at_the_first
_binary_because_of_underline_at_the_first
<



# In Erlang blow line will convert to:
# {list2 ,[{bar, baz}, {qux, true}]}
# because there is a key-value in list value of list2 will be proplist.
> list2
bar = baz
qux
<
```
Parsing file in Erlang shell:
```erlang
Erlang/OTP 19 [erts-8.3] [source-d5c06c6] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V8.3  (abort with ^G)
1> cfg:parse("test.cfg").
{ok,[{foo,bar},
     {list,[true,false,0,3.14,-1,atom,
            other_atom_because_of_underline,atom@with@at@sing,
            <<"binary_because_of_qoutation">>,
            <<"Binary_because_of_B_at_the_first">>,
            <<"@binary_because_of_@_at_the_first">>,
            <<"_binary_because_of_underline_at_the_first">>]},
     {list2,[{bar,baz},{qux,true}]}]}

%% For examle i will add values of "test.cfg" in environment variables of cfg app:
2> cfg:parse("test.cfg", {app, cfg}).
ok

3> application:get_env(cfg, foo).
{ok,bar}

4> application:get_all_env(cfg). 
[{list,[true,false,0,3.14,-1,atom,
        other_atom_because_of_underline,atom@with@at@sing,
        <<"binary_because_of_qoutation">>,
        <<"Binary_because_of_B_at_the_first">>,
        <<"@binary_because_of_@_at_the_first">>,
        <<"_binary_because_of_underline_at_the_first">>]},
 {foo,bar},
 {list2,[{bar,baz},{qux,true}]}]

%% For example i will insert values of "test.cfg" in ETS table "my_config"
%% If table does not exist, cfg will create it, if exists, should be public.
5> cfg:parse("test.cfg", {ets, my_config}).
ok

6> ets:tab2list(my_config).   
[{list2,[{bar,baz},{qux,true}]},
 {list,[true,false,0,3.14,-1,atom,
        other_atom_because_of_underline,atom@with@at@sing,
        <<"binary_because_of_qoutation">>,
        <<"Binary_because_of_B_at_the_first">>,
        <<"@binary_because_of_@_at_the_first">>,
        <<"_binary_because_of_underline_at_the_first">>]},
 {foo,bar}]

%% Callback can be {Mod, Func} which Func should accept 3 arguments.
%% It can be a fun with arity 3 too.
%% First argument is Key, second is its value and last argument is line number of data.
%% Here i make a fun, and send key, value and line number to myself as Erlang message.
7> F = fun(Key, Val, LineNumber) -> self() ! {Key, Val, LineNumber} end.
#Fun<erl_eval.18.118419387>

8> cfg:parse("test.cfg", {callback, F}).                                
ok

9> flush().
Shell got {foo,bar,5}
Shell got {list,[true,false,0,3.14,-1,atom,other_atom_because_of_underline,
                 atom@with@at@sing,<<"binary_because_of_qoutation">>,
                 <<"Binary_because_of_B_at_the_first">>,
                 <<"@binary_because_of_@_at_the_first">>,
                 <<"_binary_because_of_underline_at_the_first">>],
                22}
Shell got {list2,[{bar,baz},{qux,true}],42}
ok

%% In "test.cfg" i change "foo = bar" to "foo = " for getting error
10> {error, Reason} = cfg:parse("test.cfg", {callback, F}).
{error,{value_not_found,[{line,5},
                         {variable,foo},
                         {file,"test.cfg"}]}}

11> io:format("~s", [cfg:format_error(Reason)]).
Error: value not found
Details:
        line: 5
        variable: foo
        file: "test.cfg"
ok
```

## License
`BSD 3-Clause`
