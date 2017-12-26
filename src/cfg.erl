%%% ------------------------------------------------------------------------------------------------
%%% "cfg" is available for use under the following license, commonly known as the 3-clause
%%% (or "modified") BSD license:
%%%
%%% Copyright (c) 2017-2018, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted
%%% provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice, this list of
%%%    conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%%    conditions and the following disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%%    endorse or promote products derived from this software without specific prior written
%%%    permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ------------------------------------------------------------------------------------------------
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version  17.12.26
%% @doc
%%           Simple and modern config parser.
%% @end
%% -------------------------------------------------------------------------------------------------
-module(cfg).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([parse/1
        ,parse/2
        ,format_error/1]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(is_filename(Arg), (erlang:is_list(Arg) orelse erlang:is_binary(Arg))).
-define(DEF_TAB_OPTS, [named_table]).

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
parse(file:filename()) ->
    {'ok', []} | {'ok', [{atom(), term()}]} | {'error', any()}.
%% @doc
%%      Reads file File and returns containing data as erlang terms.
%% @end
parse(File) when ?is_filename(File) ->
    case cfg_parser:parse_file(File) of
        {ok, Data} ->
            {ok, [{Key, Val} || {Key, Val, _} <- Data]};
        {error, _}=Err ->
            Err
    end.


-spec
parse(file:filename(), Opts) ->
    'ok' | {'error', any()}
when
    Opts :: [] | [Opt],
    Opt :: {'ets', atom()}
         | {'app', atom()}
         | {'ets', atom(), TabOpts::list()}
         | {'app', atom(), SetEnvOpts::list()}
         | {'callback', {module(), Func::atom()}}
         | {'callback', fun((Key::atom()
                            ,Val::term()
                            ,LineNumber::pos_integer()) -> {'error', any()} | any())}.
%% @doc
%%      Reads file File and inserts containing data in ETS table or sets as application environment
%%      variable or calls callback with them.
%% @end
parse(File, {ets, Tab}=TabArg) when ?is_filename(File) andalso erlang:is_atom(Tab) ->
    case create_table(Tab, ?DEF_TAB_OPTS) of
        ok ->
            do_parse(File, TabArg);
        {error, _}=Err ->
            Err
    end;
parse(File, {app, App}=AppArg) when ?is_filename(File) andalso erlang:is_atom(App) ->
    do_parse(File, AppArg);
parse(File, {ets, Tab, TabOpts}) when ?is_filename(File) andalso
                                      erlang:is_atom(Tab) andalso
                                      erlang:is_list(TabOpts) ->
    case create_table(Tab, ?DEF_TAB_OPTS ++ TabOpts) of
        ok ->
            do_parse(File, {ets, Tab});
        {error, _}=Err ->
            Err
    end;
parse(File, {app, App, SetEnvOpts}=AppArg) when ?is_filename(File) andalso
                                                erlang:is_atom(App) andalso
                                                erlang:is_list(SetEnvOpts) ->
    do_parse(File, AppArg);
parse(File, {callback, Mod, Func}=CallbackArg) when ?is_filename(File) andalso
                                                    erlang:is_atom(Mod) andalso
                                                    erlang:is_atom(Func) ->
    do_parse(File, CallbackArg);
parse(File, {callback, Func}=CallbackArg) when ?is_filename(File) andalso
                                               erlang:is_function(Func, 3) ->
    do_parse(File, CallbackArg).


-spec
format_error({atom(), [] | [{atom(), term()}]}) ->
    binary().
%% @doc
%%      Formats returned error of parse/1 and parse/2 binary for making it more human-readable.
%% @end
format_error({Reason, ErrParams}) when erlang:is_atom(Reason) andalso erlang:is_list(ErrParams) ->
    Fold =
        fun
            ({Key, Val}, Acc) when erlang:is_atom(Val) ->
                Acc ++ "\t" ++ erlang:atom_to_list(Key) ++ ": " ++ erlang:atom_to_list(Val) ++ "\n";
            ({Key, Val}, Acc) ->
                Acc ++ "\t" ++ erlang:atom_to_list(Key) ++ ": " ++ io_lib:print(Val) ++ "\n"
        end,
    lists:foldl(Fold, "Error: " ++ erlang:atom_to_list(Reason) ++ "\nDetails:\n", ErrParams).

%% -------------------------------------------------------------------------------------------------
%% Internal functions:

create_table(Tab, Opts) ->
    case lists:member(Tab, ets:all()) of
        true ->
            Self = erlang:self(),
            case {ets:info(Tab, protection), ets:info(Tab, owner)} of
                {public, _} ->
                    ok;
                {_, Self} ->
                    ok;
                {Protection, Owner} ->
                    {error, {table, [{protection, Protection}
                                    ,{owner, Owner}
                                    ,{table, Tab}
                                    ,{self, Self}]}}
            end;
        false ->
            try
                _ = ets:new(Tab, Opts),
                ok
            catch
                _ErrType:Reason ->
                    {error, {create_table, [{reason, Reason}
                                           ,{stacktrace, erlang:get_stacktrace()}]}}
            end
    end.


do_parse(File, InsertMode) ->
    case cfg_parser:parse_file(File) of
        {ok, Data} ->
            insert(Data, File, InsertMode);
        {error, _}=Err ->
            Err
    end.


insert(Data, File, InsertMode) ->
    Func =
        case InsertMode of
            {ets, Tab} ->
                fun({Key, Val, LineNumber}) ->
                    try
                        ets:insert(Tab, {Key, Val})
                    catch
                        _:Reason ->
                            {error, {insert, [{reason, Reason}
                                             ,{key, Key}
                                             ,{value, Val}
                                             ,{line, LineNumber}
                                             ,{table, Tab}
                                             ,{file, File}
                                             ,{stacktrace, erlang:get_stacktrace()}]}}
                    end
                end;
            {app, AppName} ->
                fun({Key, Val, LineNumber}) ->
                    try
                        application:set_env(AppName, Key, Val)
                    catch
                        _:Reason ->
                            {error, {insert, [{reason, Reason}
                                             ,{key, Key}
                                             ,{value, Val}
                                             ,{line, LineNumber}
                                             ,{application, AppName}
                                             ,{file, File}
                                             ,{stacktrace, erlang:get_stacktrace()}]}}
                    end

                end;
            {app, AppName, SetEnvOpts} ->
                fun({Key, Val, LineNumber}) ->
                    try
                        application:set_env(AppName, Key, Val, SetEnvOpts)
                    catch
                        _:Reason ->
                            {error, {insert, [{reason, Reason}
                                             ,{key, Key}
                                             ,{value, Val}
                                             ,{line, LineNumber}
                                             ,{application, AppName}
                                             ,{options, SetEnvOpts}
                                             ,{file, File}
                                             ,{stacktrace, erlang:get_stacktrace()}]}}
                    end
                end;
            {callback, Mod, Func2} ->
                fun({Key, Val, LineNumber}) ->
                    try
                        case Mod:Func2(Key, Val, LineNumber) of
                            {error, Reason} ->
                                {error, {insert, [{reason, Reason}
                                                 ,{key, Key}
                                                 ,{value, Val}
                                                 ,{line, LineNumber}
                                                 ,{module, Mod}
                                                 ,{function, Func2}
                                                 ,{file, File}]}};
                            _ ->
                                ok
                        end
                    catch
                        _:Reason2 ->
                            {error, {insert, [{reason, Reason2}
                                             ,{key, Key}
                                             ,{value, Val}
                                             ,{line, LineNumber}
                                             ,{module, Mod}
                                             ,{function, Func2}
                                             ,{file, File}
                                             ,{stacktrace, erlang:get_stacktrace()}]}}
                    end
                end;
            {callback, Func2} ->
                fun({Key, Val, LineNumber}) ->
                    try
                        case Func2(Key, Val, LineNumber) of
                            {error, Reason} ->
                                {error, {insert, [{reason, Reason}
                                                 ,{key, Key}
                                                 ,{value, Val}
                                                 ,{line, LineNumber}
                                                 ,{function, Func2}
                                                 ,{file, File}]}};
                            _ ->
                                ok
                        end
                    catch
                        _:Reason2 ->
                            {error, {insert, [{reason, Reason2}
                                             ,{key, Key}
                                             ,{value, Val}
                                             ,{line, LineNumber}
                                             ,{function, Func2}
                                             ,{file, File}
                                             ,{stacktrace, erlang:get_stacktrace()}]}}
                    end
                end
        end,
    do_insert(Data, Func).


do_insert([Data|Rest], Func) ->
    case Func(Data) of
        {error, _}=Err ->
            Err;
        _ ->
            do_insert(Rest, Func)
    end;
do_insert([], _) ->
    ok.