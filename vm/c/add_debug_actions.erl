#!/usr/bin/env escript
%%! -smp enable -sname peg_debugger

-export([process_line/2]).

main([InputFile, OutputFile]) ->
    {ok, RawGrammar} = read_file_on_stdin(InputFile),
    Grammar = binary_to_list(RawGrammar),
    ModifiedGrammar = add_actions(re:split(Grammar, "\n", [{return, list}])),
    io:format("~s\n", ModifiedGrammar).

main(_Args) ->
    {ok, Binary} = io:read_all_standard_input(),
    % Now do something with the Binary
    io:format("Content: ~s~n", [Binary]).

read_file_on_stdin() ->
    read_file_on_stdin(<<>>).

read_file_on_stdin(Acc) -
    case io:get_line('') of
        eof ->
            {ok, Acc};
        Line ->
            read_file_on_stdin(<<Acc/binary, Line/binary>>)
    end.

add_actions([]) ->
    "";
add_actions([Line|Rest]) ->
    case re:run(Line, "(\\w+)\\s*<-\\s*(.*)", [{capture, all_but_first, list}]) of
        {match, [RuleName, _]} ->
            {RemainingLines, MoreLines} = until_next_rule(Rest),
            DebugAction =
                io_lib:format(" { fprintf(stderr, \"Matched ~s\\n\"); }\n", [RuleName]),
            Line ++ MoreLines ++ DebugAction ++ add_actions(RemainingLines);
        nomatch ->
            Line ++ "\n" ++ add_actions(Rest)
    end.

until_next_rule(Lines) ->
    until_next_rule(Lines, "").

until_next_rule(["#" ++ _|Rest], MoreLines) ->
    until_next_rule(Rest, MoreLines);
until_next_rule([""|Rest], MoreLines) ->
    until_next_rule(Rest, MoreLines);
until_next_rule([], MoreLines) ->
    {[], string:chomp(MoreLines)};
until_next_rule([Line|Rest], MoreLines) ->
    case re:run(Line, "(\\w+)\\s*<-\\s*(.*)", [{capture, all_but_first, list}]) of
        {match, _} ->
            {[Line|Rest], MoreLines};
        _ ->
            until_next_rule(Rest, MoreLines ++ "\n" ++ Line)
    end.
