-module(parse2).
-export([indented_tree/1]).

-record(node, {name, value, children = []}).

indented_tree(Filename) ->
    {ok, Content} = file:read_file(Filename),
    {[Node], _} =
        indented_tree(string:split(binary_to_list(Content), "\n", all), 0),
    Node.

indented_tree([], _Level) -> {[], []};
indented_tree([""|Rest], Level) ->
    indented_tree(Rest, Level);
indented_tree([Line|Rest] = Lines, Level) ->
    case parse_line(Line) of
        {Level, Name, Value} ->
            {Children, SiblingLines} = indented_tree(Rest, Level + 1),
            {Siblings, RestLines} = indented_tree(SiblingLines, Level),
            {[#node{name = Name, value = Value, children = Children}|
              Siblings], RestLines};
        _ ->
            {[], Lines}
    end.

parse_line(Line) ->
    case string:lexemes(string:trim(Line), [$:]) of
        [Name, Value] ->
            {calc_indent(Line), Name, Value};
        [Name] ->
            {calc_indent(Line), Name, undefined}
    end.

calc_indent(Line) -> calc_indent(Line, 0) div 2.

calc_indent([$ |Rest], N) -> calc_indent(Rest, N+1);
calc_indent(_, N) -> N.
