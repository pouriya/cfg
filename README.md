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
# comment


var =


> list
1
3.14
atom
'atom'
other@atom
Binary
"string"
<


> proplist
var = value
<


> #map
key = value
<
```
Parsing file in Erlang shell:
```erlang
Erlang/OTP 19 [erts-8.3] [source-d5c06c6] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V8.3  (abort with ^G)
1> cfg:parse("test.cfg").
{ok,[{var,value},
     {list,[1,3.14,atom,atom,other@atom,<<"Binary">>,"string"]},
     {proplist,[{var,value}]},
     {map,#{key => value}}]}

2> cfg:parse("test.cfg", {ets, config}).
ok

3> ets:tab2list(config).
[{list,[1,3.14,atom,atom,other@atom,<<"Binary">>,"string"]},
 {proplist,[{var,value}]},
 {var,value},
 {map,#{key => value}}]

4> application:get_all_env(cfg).
[]

5> cfg:parse("test.cfg", {app, cfg}).   
ok

6> application:get_all_env(cfg).     
[{var,value},
 {proplist,[{var,value}]},
 {list,[1,3.14,atom,atom,other@atom,<<"Binary">>,"string"]},
 {map,#{key => value}}]

7> Callback = fun(Var, Val, Line) -> self() ! {Line, Var, Val} end.
#Fun<erl_eval.18.118419387>

8> cfg:parse("test.cfg", {callback, Callback}).
ok

9> flush().
Shell got {4,var,value}
Shell got {7,list,[1,3.14,atom,atom,other@atom,<<"Binary">>,"string"]}
Shell got {18,proplist,[{var,value}]}
Shell got {23,map,#{key => value}}
ok

%% I remove variable 'var' in line 4 for getting error
10> {error, Rsn} = cfg:parse("test.cfg").
{error,{value_not_found,[{line,4},
                         {variable,var},
                         {file,"test.cfg"}]}}

11> io:format( cfg:format_error(Rsn) ).  
Error: value_not_found
Details:
        line: 4
        variable: var
        file: "test.cfg"
ok
```

## `License`
`BSD 3-Clause`

## `Author`
`pouriya.jahanbakhsh@gmail.com`

## `Hex version`
[**17.12.26**](https://hex.pm/packages/cfg)