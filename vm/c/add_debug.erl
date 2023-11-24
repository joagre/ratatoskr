#!/usr/bin/env escript
%%! -smp enable -sname peg_debugger

main([InputFile, OutputFile]) ->
    {ok, RawGrammar} = file:read_file(InputFile),
    Grammar = binary_to_list(RawGrammar),
    {ModifiedGrammar, _} = lists:foldl(fun process_line/2, {[], outside_rule}, re:split(Grammar, "\n", [{return, list}])),
    ok = file:write_file(OutputFile, string:join(ModifiedGrammar, "\n")),
    io:format("Modified grammar written to ~s~n", [OutputFile]).

process_line(Line, {Acc, outside_rule}) ->
    case re:run(Line, "(\\w+)\\s*<-\\s*(.*)", [{capture, all_but_first, list}]) of
        {match, [RuleName, _]} ->
            DebugAction = io_lib:format(" { fprintf(stderr, \"Matched ~s\\n\"); }", [RuleName]),
            {[Line ++ DebugAction | Acc], inside_rule};
        nomatch ->
            {[Line | Acc], outside_rule}
    end;
process_line(Line, {Acc, inside_rule}) ->
    if
        re:run(Line, "^\\s*[^\\s#]") =/= nomatch ->
            % Continuation of a rule, keep inside_rule state
            {Acc, inside_rule};
        true ->
            % Line doesn't start with non-whitespace (or it's a comment), outside of a rule now
            {Acc, outside_rule}
    end.
