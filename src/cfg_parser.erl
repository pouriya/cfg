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
%% -------------------------------------------------------------------------------------------------
-module(cfg_parser).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% API:
-export([parse_file/1]).

%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(is_filename(Arg), (erlang:is_list(Arg) orelse erlang:is_binary(Arg))).

%% -------------------------------------------------------------------------------------------------
%% API:

-spec
parse_file(file:filename()) ->
    {'ok', []} | {'ok', [{atom(), term(), pos_integer()}]} | {'error', any()}.
parse_file(File) when ?is_filename(File) ->
    case read_file(File) of
        {ok, Data} ->
            case parse_lines(separate_lines(Data)) of
                {ok, _}=Ok ->
                    Ok;
                {error, {Reason, ErrParams}} ->
                    {error, {Reason, ErrParams ++ [{file, File}]}}
            end;
        {error, _}=Err ->
            Err
    end.

%% -------------------------------------------------------------------------------------------------
%% Internal functions:

read_file(File) ->
    case file:read_file(File) of
        {ok, _}=Ok ->
            Ok;
        {error, Reason} ->
            {error, {file, [{reason, Reason}
                           ,{info, file:format_error(Reason)}
                           ,{file, File}]}}
    end.


parse_lines(Lines) ->
    parse_lines(remove_commnet_lines(Lines), undefined, []).


separate_lines(Data) ->
    separate_lines(Data, <<>>, 1, []).


separate_lines(<<"\r\n", Rest/bits>>, <<>>, Count, Lines) ->
    separate_lines(Rest, <<>>, Count+1, Lines);
separate_lines(<<"\r\n", Rest/bits>>, Buffer, Count, Lines) ->
    separate_lines(Rest, <<>>, Count+1, [{Count, Buffer}|Lines]);
separate_lines(<<"\n", Rest/bits>>, <<>>, Count, Lines) ->
    separate_lines(Rest, <<>>, Count+1, Lines);
separate_lines(<<"\n", Rest/bits>>, Buffer, Count, Lines) ->
    separate_lines(Rest, <<>>, Count+1, [{Count, Buffer}|Lines]);
separate_lines(<<Char:8/bits, Rest/bits>>, Buffer, Count, Lines) ->
    separate_lines(Rest, <<Buffer/bits, Char/bits>>, Count, Lines);
separate_lines(<<>>, <<>>, _Count, Lines) ->
    lists:reverse(Lines);
separate_lines(<<>>, Buffer, Count, Lines) ->
    lists:reverse([{Count, Buffer}|Lines]).


remove_commnet_lines(Lines) ->
    [Line || {_LineNumber, LineData}=Line <- Lines, not is_comment_line(LineData)].


is_comment_line(<<$#:8, _/bits>>) ->
    true;
is_comment_line(_) ->
    false.


parse_lines([{LineNumber, LineData}|Lines], undefined, Ret) ->
    case parse_line(LineData) of
        {ok, {var, '', Val}} ->
            {error, {variable_not_found, [{line, LineNumber}, {value, Val}]}};
        {ok, {var, Var, ''}} ->
            {error, {value_not_found, [{line, LineNumber}, {variable, Var}]}};
        {ok, {var, Var, Val}} ->
            case lists:keyfind(Var, 1, Ret) of
                false ->
                    parse_lines(Lines, undefined, [{Var, Val, LineNumber}|Ret]);
                {_, Val2, LineNumber2} ->
                    {error, {repeated_var, [{var, Var}
                                           ,{line, LineNumber}
                                           ,{value, Val}
                                           ,{info, "repeated in line " ++
                                                   erlang:integer_to_list(LineNumber2) ++
                                                   " with value " ++
                                                   io_lib:print(Val2)}]}}
            end;
        {ok, {start_list, ''}} ->
            {error, {list_not_found, [{line, LineNumber}]}};
        {ok, {start_list, ListName}} ->
            case lists:keyfind(ListName, 1, Ret) of
                false ->
                    parse_lines(Lines, {ListName, [], LineNumber}, Ret);
                {_, Val2, LineNumber2} ->
                    {error, {repeated_list, [{list_name, ListName}
                                            ,{line, LineNumber}
                                            ,{info, "repeated in line " ++
                                                    erlang:integer_to_list(LineNumber2) ++
                                                    " with value " ++
                                                    io_lib:print(Val2)}]}}
            end;
        {ok, {list_value, Val}} ->
            {error, {syntax, [{reason, "found unwanted list or map value"}
                             ,{value, Val}
                             ,{line, LineNumber}]}};
        {ok, end_list} ->
            {error, {syntax, [{reason, "found unwanted closing list or map character"}
                             ,{line, LineNumber}]}};
        {error, Reason} ->
            {error, {syntax, [{reason, Reason}
                             ,{line, LineNumber}]}}
    end;
parse_lines([{LineNumber, LineData}|Lines], {ListName, ListVals, ListLineNumber}, Ret) ->
    case parse_line(LineData) of
        {ok, {list_value, Val}} ->
            parse_lines(Lines, {ListName, [Val|ListVals], ListLineNumber}, Ret);
        {ok, end_list} ->
            ListVals2 = maybe_convert_to_proplist(lists:reverse(ListVals)),
            {ListName2, ListVals3} = maybe_convert_to_map(ListName, ListVals2),
            parse_lines(Lines
                       ,undefined
                       ,[{ListName2, ListVals3, ListLineNumber} | Ret]);
        {ok, {var, '', Val}} ->
            {error, {variable_not_found, [{line, LineNumber}, {value, Val}]}};
        {ok, {var, Var, ''}} ->
            {error, {value_not_found, [{line, LineNumber}, {variable, Var}]}};
        {ok, {var, Var, Val}} ->
            parse_lines(Lines, {ListName, [{Var, Val}|ListVals], ListLineNumber}, Ret);
        {ok, {start_list, ListNam2}} ->
            {error, {syntax, [{reason, "found unwanted list or map declaration"}
                             ,{variable, ListNam2}
                             ,{line, LineNumber}]}};
        {error, Reason} ->
            {error, {syntax, [{reason, Reason}
                             ,{line, LineNumber}]}}
    end;
parse_lines([], undefined, Ret) ->
    {ok, lists:reverse(Ret)};
parse_lines([], {ListName, _, ListLineNumber}, _Ret) ->
    {error, {syntax, [{reason, "found unclosed list or map"}
                     ,{variable, ListName}
                     ,{line, ListLineNumber}]}}.


parse_line(<<$>:8, Rest/bits>>) ->
    {ok, {start_list, trim(Rest)}};
parse_line(<<$<:8, _/bits>>) ->
    {ok, end_list};
parse_line(Line) ->
    case binary:split(Line, <<"=">>) of
        [Var, Val] ->
            {ok, {var, decode(trim(Var)), decode(trim(Val))}};
        _ ->
            {ok, {list_value, decode(trim(Line))}}
    end.


trim(Arg) ->
    re:replace(Arg, <<"(^\\s+)|(\\s+$)">>, <<"">>, [global, {return, binary}]).


to_variable(Arg) ->
    erlang:binary_to_atom(Arg, utf8).


decode(Arg) ->
    BoolRes =
        if
            Arg == <<"true">> ->
                true;
            Arg == <<"false">> ->
                false;
            true ->
                undefined
        end,
    IntRes =
        case BoolRes of
            undefined ->
                try
                    erlang:binary_to_integer(Arg)
                catch
                    _:_ ->
                        undefined
                end;
            _ ->
                BoolRes
        end,
    FloatRes =
        case IntRes of
            undefined ->
                try
                    erlang:binary_to_float(Arg)
                catch
                    _:_ ->
                        undefined
                end;
            _ ->
                IntRes
        end,
    AtomRes =
        case FloatRes of
            undefined ->
                try
                    binary_to_atom2(Arg)
                catch
                    _:_ ->
                        undefined
                end;
            _ ->
                FloatRes
        end,
    case AtomRes of
        undefined ->
            case {binary:first(Arg), binary:last(Arg)} of
                {$", $"} when erlang:byte_size(Arg) > 1 ->
                    erlang:binary_to_list(binary:part(Arg, 1, erlang:byte_size(Arg)-2));
                {$', $'} when erlang:byte_size(Arg) > 1 ->
                    erlang:binary_to_atom(binary:part(Arg, 1, erlang:byte_size(Arg)-2), utf8);
                _ ->
                    Arg
            end;
        _ ->
            AtomRes
    end.


binary_to_atom2(Arg) ->
    binary_to_atom2(Arg, <<>>).


binary_to_atom2(<<Char:8/integer, Rest/bits>>, Buffer) when (Char >= 97 andalso Char =< 122) orelse
                                                            (Char >= 65 andalso Char =< 90) orelse
                                                            (Char >= 48 andalso Char =< 57) orelse
                                                            Char == 95 orelse % $_
                                                            Char == 64 -> % $@
    binary_to_atom2(Rest, <<Buffer/bits, Char>>);
binary_to_atom2(<<>>, <<Char:8/integer, _/bits>>) when (Char >= 65 andalso Char =< 90) ->
    erlang:error(badarg);
binary_to_atom2(<<>>, <<Char:8/integer, _/bits>>) when (Char >= 48 andalso Char =< 57) ->
    erlang:error(badarg);
binary_to_atom2(<<>>, <<$_, _/bits>>) ->
    erlang:error(badarg);
binary_to_atom2(<<>>, <<$@, _/bits>>) ->
    erlang:error(badarg);
binary_to_atom2(<<>>, Buffer) ->
    erlang:binary_to_atom(Buffer, utf8);
binary_to_atom2(_, _) ->
    erlang:error(badarg).


maybe_convert_to_proplist(List) ->
    maybe_convert_to_proplist(List, List).



maybe_convert_to_proplist([{_, _}|_], List2) ->
    [if
         erlang:is_tuple(Item) ->
             Item;
         true ->
             {Item, true}
     end || Item <- List2];
maybe_convert_to_proplist([_|List], List2) ->
    maybe_convert_to_proplist(List, List2);
maybe_convert_to_proplist([], List2) ->
    List2.


maybe_convert_to_map(<<$#, Rest/binary>>, ListVals) ->
    {to_variable(Rest), to_map(ListVals, #{})};
maybe_convert_to_map(ListName, ListVals) ->
    {to_variable(ListName), ListVals}.


to_map([{Key, Val}|Rest], Map) ->
    to_map(Rest, Map#{Key => Val});
to_map([Key|Rest], Map) ->
    to_map(Rest, Map#{Key => true});
to_map([], Map) ->
    Map.
