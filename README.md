[![Hex version](https://img.shields.io/hexpm/v/cfg.svg "Hex version")](https://hex.pm/packages/cfg)

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

foo = bar

> list
true
false
0
3.14
-1
atom
other_atom_because_of_underline
atom@with@at@sing
'atom because of apostrophe'
"string_because_of_qoutation"
Binary_because_of_B_at_the_first
@binary_because_of_@_at_the_first
_binary_because_of_underline_at_the_first
<

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
            'atom because of apostrophe',"string_because_of_qoutation",
            <<"Binary_because_of_B_at_the_first">>,
            <<"@binary_because_of_@_at_the_first">>,
            <<"_binary_because_of_underline_at_the_firs"...>>]},
     {list2,[{"foo",bar},{qux,true}]}]}

2> cfg:parse("test.cfg", {app, cfg}).
ok

3> application:get_env(cfg, foo).
{ok,bar}
 
4> cfg:parse("test.cfg", {ets, my_config}).
ok

5> ets:tab2list(my_config).
[{list2,[{"foo",bar},{qux,true}]},
 {list,[true,false,0,3.14,-1,atom,
        other_atom_because_of_underline,atom@with@at@sing,
        'atom because of apostrophe',"string_because_of_qoutation",
        <<"Binary_because_of_B_at_the_first">>,
        <<"@binary_because_of_@_at_the_first">>,
        <<"_binary_because_of_underline_at_the_first">>]},
 {foo,bar}]

6> %% Callback can be {Mod, Func} which Func should accept 3 arguments.
6> %% It can be a fun with arity 3 too.
6> %% First argument is Key, second is its value and last argument is line number of data.
6> %% Here i make a fun, and send key, value and line number to myself as Erlang message.
6> F = fun(Key, Val, LineNumber) -> self() ! {Key, Val, LineNumber} end.   
#Fun<erl_eval.18.52032458>

7> cfg:parse("test.cfg", {callback, F}).                                
ok

8> flush().
Shell got {foo,bar,5}
Shell got {list,[true,false,0,3.14,-1,atom,other_atom_because_of_underline,
                 atom@with@at@sing,'atom because of apostrophe',
                 "string_because_of_qoutation",
                 <<"Binary_because_of_B_at_the_first">>,
                 <<"@binary_because_of_@_at_the_first">>,
                 <<"_binary_because_of_underline_at_the_first">>],
                23}
Shell got {list2,[{"foo",bar},{qux,true}],42}
ok

%% In "test.cfg" i change "foo = bar" to "foo = " for getting error
9> {error, Rsn} = cfg:parse("test.cfg").                                                                 
{error,{value_not_found,[{line,3},
                         {variable,foo},
                         {file,"test.cfg"}]}}

10> io:format(cfg:format_error(Rsn)).
Error: value_not_found
Details:
        line: 3
        variable: foo
        file: "test.cfg"
ok
```

## License
`BSD 3-Clause`

## Author
`pouriya.jahanbakhsh@gmail.com`