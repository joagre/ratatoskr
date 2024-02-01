-module(lint).
-compile(export_all).

-include("lint.hrl").

start() ->
    start("../../../bin/lint", "../../../examples/sa/hm1.sa").

start(Filename) ->
    start("../../../bin/lint", Filename).

start(Lint, Filename) ->
    Source = os:cmd("cat -n " ++ Filename),
    Result = os:cmd(Lint ++ " < " ++ Filename),
    Lines = string:split(Result, "\n", all),
    {Tree, Equations} = split_parts(Lines),
    {[Node], []} = parse_tree(Tree, 0),
    {Source, Result, Node, parse_equations(Equations)}.

split_parts(Lines) ->
    split_parts(Lines, []).

split_parts([""|Rest], Acc) ->
    {lists:reverse(Acc), Rest};
split_parts([Line|Rest], Acc) ->
    split_parts(Rest, [Line|Acc]).

%%
%% Parse tree
%%

parse_tree([], _Level) ->
    {[], []};
parse_tree([""|Rest], Level) ->
    parse_tree(Rest, Level);
parse_tree([Line|Rest] = Lines, Level) ->
    case parse_line(Line) of
        {Name, Row, Value, Type, Level} ->
            {Children, SiblingLines} = parse_tree(Rest, Level + 1),
            {Siblings, RestLines} = parse_tree(SiblingLines, Level),
            {[#node{name = Name,
                    row = list_to_integer(Row),
                    value = Value,
                    type = parse_node_type(string:trim(Type)),
                    children = Children}|Siblings],
             RestLines};
        _ ->
            {[], Lines}
    end.

parse_node_type("") -> undefined;
parse_node_type("int") -> int;
parse_node_type("bool") -> bool;
parse_node_type("t" ++ Variable) -> list_to_integer(Variable).

parse_line(Line) ->
    [Name, Row, Value, Type] = string:split(string:trim(Line), ":", all),
    {Name, Row, Value, Type, calc_indent(Line)}.

calc_indent(Line) -> calc_indent(Line, 0) div 2.

calc_indent([$ |Rest], N) -> calc_indent(Rest, N + 1);
calc_indent(_, N) -> N.

%%
%% Parse equations
%%

parse_equations([""]) ->
    [];
parse_equations([Line|Rest]) ->
    [OriginNodeName, OriginRow, Row, Value, Type] =
        string:split(Line, ":", all),
    [#equation{origin_node_name = OriginNodeName,
               origin_row = list_to_integer(OriginRow),
               row = list_to_integer(Row),
               value = Value,
               type = parse_type(Type)}|
     parse_equations(Rest)].

parse_type(Line) ->
    {ok, Tokens, _} = erl_scan:string(Line ++ "."),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    [Expr] = Parsed,
    {value, Result, _} = erl_eval:exprs([Expr], erl_eval:new_bindings()),
    Result.
