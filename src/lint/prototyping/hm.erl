-module(hm).
-compile(export_all).

-include("lint.hrl").

%% foo f g x = if f(x == 1) then g(x) else 20
simple_test() ->
    Equations = [{int, int},
                 {3, int},
                 {6, bool},
                 {1, {[6], 5}},
                 {2, {[3], 7}},
                 {5, bool},
                 {4, 7},
                 {4, int},
                 {0, {[1, 2, 3], 4}}],
    Node = #node{},
    Substitutions = unify_all_equations(Equations, Node, maps:new()),
    {[{[bool],bool},{[int],int},int],int} =
        dereference(Substitutions, 0).

hm1() ->
    test("../../../examples/sa/hm1.sa").

test_literals() ->
    test("../../../examples/sa/test_literals.sa").

test(Filename) ->
    case lint:start(Filename) of
        {error, Reason} ->
            Reason;
        Result ->
            print_test_result(Result)
    end.

print_test_result({Source, Result, Node, AdornedEquations}) ->
    io:format("==== Source:\n~s\n", [Source]),
    io:format("==== Lint output:\n~s\n", [Result]),
    %%io:format("==== Node:\n~p\n\n", [Node]),
    %%io:format("==== Adorned equations:\n~p\n\n", [AdornedEquations]),
    Equations =
        lists:map(fun(#equation{type = Type}) -> Type end, AdornedEquations),
    %%io:format("==== Equations:\n~p\n", [Equations]),
    io:format("==== Equations:\n"),
    lists:foreach(
      fun(#equation{type = {Left, Right}}) ->
              io:format("~s = ~s\n",
                        [type_to_string(Left), type_to_string(Right)])
      end, AdornedEquations),
    io:format("\n"),
    case unify_all_equations(Equations, Node, maps:new()) of
        {mismatch, TypeStack} ->
            io:format("==== Type error:\n~s",
                      [format_type_stack(TypeStack)]),
            %%io:format("==== Type error:\n~s\n",
            %%          [prettify_type_stack(TypeStack)]),
            ok;
        Substitutions ->
            %%io:format("==== Substitutions:\n~p\n", [Substitutions]),
            io:format("==== Solutions:\n"),
            lists:foreach(
              fun(#equation{type = {Left, Right}}) ->
                      io:format("~s -> ~s\n",
                                [type_to_string(Left),
                                 type_to_string(
                                   dereference(Substitutions, Right))])
              end, AdornedEquations)
    end.

%%
%% Unify all equations
%%

unify_all_equations([], _Node, Substitutions) ->
    Substitutions;
unify_all_equations([{Left, Right}|Rest], Node, Substitutions) ->
    case unify(Left, Right, [{Left, Right}], Node, Substitutions) of
        {mismatch, TypeStack} ->
            {mismatch, TypeStack};
        UpdatedSubstitutions ->
            unify_all_equations(Rest, Node, UpdatedSubstitutions)
    end.

unify(_X, _Y, _TypeStack, _Node, {mismatch, TypeStack}) ->
    {mismatch, TypeStack};
unify(X, X, _TypeStack, _Node, Substitutions) ->
    Substitutions;
unify(X, Y, TypeStack, Node, Substitutions) when is_integer(X) ->
    unify_variable(X, Y, TypeStack, Node, Substitutions);
unify(X, Y, TypeStack, Node, Substitutions) when is_integer(Y) ->
    unify_variable(Y, X, TypeStack, Node, Substitutions);
unify({list, X}, {list, Y}, TypeStack, Node, Substitutions) ->
    unify_args([X], [Y], TypeStack, Node, Substitutions);
unify({tuple, Xs}, {tuple, Ys}, TypeStack, Node, Substitutions) ->
    case length(Xs) /= length(Ys) of
        true ->
            {mismatch, TypeStack};
        false ->
            unify_args(Xs, Ys, TypeStack, Node, Substitutions)
    end;
unify({map, KeyX, ValueX}, {map, KeyY, ValueY}, TypeStack, Node,
      Substitutions) ->
    unify_args([KeyX, ValueX], [KeyY, ValueY], TypeStack, Node, Substitutions);
unify({constructor, Xs}, {constructor, Ys}, TypeStack, Node, Substitutions) ->
    unify_args(Xs, Ys, TypeStack, Node, Substitutions);
unify({ArgsX, ReturnX}, {ArgsY, ReturnY}, TypeStack, Node, Substitutions) ->
    case length(ArgsX) /= length(ArgsY) of
        true ->
            {mismatch, TypeStack};
        false ->
            unify_args(ArgsX, ArgsY, TypeStack, Node,
                       unify(ReturnX, ReturnY, [{ReturnX, ReturnY}|TypeStack],
                             Node, Substitutions))
    end;
unify(_, _, TypeStack, _, _) ->
    {mismatch, TypeStack}.

unify_args([], [], _TypeStack, _MetInfo, Substitutions) ->
    Substitutions;
unify_args([X|Xs], [Y|Ys], TypeStack, Node, Substitutions) ->
    unify_args(Xs, Ys, TypeStack, Node,
               unify(X, Y, [{X, Y}|TypeStack], Node, Substitutions)).

unify_variable(Variable, Type, TypeStack, Node, Substitutions)
  when is_integer(Variable) ->
    case maps:get(Variable, Substitutions, undefined) of
        undefined when is_integer(Type) ->
            case maps:get(Type, Substitutions, undefined) of
                undefined ->
                    case occurs_check(Variable, Type, Substitutions) of
                        true ->
                            {mismatch, TypeStack};
                        false ->
                            Substitutions#{Variable => Type}
                    end;
                Substitution ->
                    unify(Variable, Substitution,
                          [{Variable, Substitution}|TypeStack], Node,
                          Substitutions)

            end;
        undefined ->
            case occurs_check(Variable, Type, Substitutions) of
                true ->
                    {mismatch, TypeStack};
                false ->
                    Substitutions#{Variable => Type}
            end;
        Substitution ->
            unify(Substitution, Type, [{Substitution, Type}|TypeStack],
                  Node, Substitutions)
    end.

occurs_check(Variable, Variable, _Substitutions) when is_integer(Variable) ->
    true;
occurs_check(Variable, Type, Substitutions)
  when is_integer(Type) andalso is_integer(Variable) ->
    case maps:get(Type, Substitutions, undefined) of
        undefined ->
            case Type of
                {ArgTypes, ReturnType} ->
                    occurs_check(Variable, ReturnType, Substitutions) orelse
                        lists:any(
                          fun(ArgType) ->
                                  occurs_check(ArgType, ArgType, Substitutions)
                          end, ArgTypes);
                _ ->
                    false
            end;
        Substitution ->
            occurs_check(Variable, Substitution, Substitutions)
    end;
occurs_check(_Variable, _Type, _Substitutions) ->
    false.

%%
%% Dereference
%%

dereference(_Substitutions, BaseType) when is_atom(BaseType) ->
    BaseType;
dereference(Substitutions, {list, X}) ->
    {list, dereference(Substitutions, X)};
dereference(Substitutions, {map, Key, Value}) ->
    {map, dereference(Substitutions, Key), dereference(Substitutions, Value)};
dereference(Substitutions, {tuple, Xs}) ->
    {tuple, lists:map(fun(Type) -> dereference(Substitutions, Type) end, Xs)};
dereference(Substitutions, {ArgTypes, ReturnType}) ->
    {lists:map(fun(Type) -> dereference(Substitutions, Type) end, ArgTypes),
     dereference(Substitutions, ReturnType)};
dereference(Substitutions, Variable) when is_integer(Variable) ->
    case maps:get(Variable, Substitutions, undefined) of
        undefined ->
            '_';
        Substitution ->
            dereference(Substitutions, Substitution)
    end.

%%
%% Format type error
%%

format_type_stack([]) ->
    "";
format_type_stack([{X, Y}|Rest]) ->
    format_type_stack(Rest) ++
        type_to_string(X) ++ " -> " ++ type_to_string(Y) ++ "\n".

type_to_string('_') ->
    "_";
type_to_string(bool) ->
    "Bool";
type_to_string(int) ->
    "Int";
type_to_string(float) ->
    "Float";
type_to_string(string) ->
    "String";
type_to_string(task) ->
    "Task";
type_to_string({constructor, Xs}) ->
    "<" ++ type_to_string(Xs) ++ ">";
type_to_string({list, X}) ->
    "[" ++ type_to_string(X) ++ "]";
type_to_string(empty_list) ->
    "[]";
type_to_string({map, Key, Value}) ->
    "[" ++ type_to_string(Key) ++ ": " ++ type_to_string(Value) ++ "]";
type_to_string(empty_map) ->
    "[:]";
type_to_string({tuple, Xs}) ->
    "(" ++ type_to_string(Xs) ++ ")";
type_to_string(empty_tuple) ->
    "()";
type_to_string({[ArgType], ReturnType}) ->
    "(" ++ type_to_string(ArgType) ++ " -> " ++
        type_to_string(ReturnType) ++ ")";
type_to_string({ArgTypes, ReturnType}) ->
    "({"++ type_to_string(ArgTypes) ++ "} -> " ++
        type_to_string(ReturnType) ++ ")";
type_to_string(TypeVariable) when is_integer(TypeVariable) ->
    "t" ++ integer_to_list(TypeVariable);
type_to_string([]) ->
    "";
type_to_string([Type]) ->
    type_to_string(Type);
type_to_string([Type|Rest]) ->
    type_to_string(Type) ++ ", " ++ type_to_string(Rest).

%% prettify_type_stack(_Node, []) ->
%%     [];
%% prettify_type_stack(Node, [{X, Y}|Rest]) ->
%%     prettify_type_stack(Node, Rest) ++
%%         prettify_type(Node, X) ++ " -> " ++ prettify_type(Node, Y) ++ "\n".

%% prettify_type(_Node, int) ->
%%     "Int";
%% prettify_type(_Node, bool) ->
%%     "Bool";
%% prettify_type(Node, TypeVariable) when is_integer(TypeVariable) ->
%%     case search_by_type(Node, TypeVariable) of
%%         #node{name = Name, row = Row, value = undefined} ->
%%             Name ++ ":" ++ integer_to_list(Row);
%%         #node{name = Name, row = Row, value = Value} ->
%%             Name ++ ":" ++ integer_to_list(Row) ++ ":" ++ Value
%%     end;
%% prettify_type(Node, {[ArgType], ReturnType}) ->
%%     "(" ++ prettify_type(Node, ArgType) ++ " -> " ++
%%         prettify_type(Node, ReturnType) ++ ")";
%% prettify_type(Node, {ArgTypes, ReturnType}) ->
%%     "((" ++ prettify_type(Node, ArgTypes) ++ ") -> " ++
%%         prettify_type(Node, ReturnType) ++ ")";
%% prettify_type(_Node, []) ->
%%     [];
%% prettify_type(Node, [Type]) ->
%%     prettify_type(Node, Type);
%% prettify_type(Node, [Type|Types]) ->
%%     prettify_type(Node, Type) ++ ", " ++ prettify_type(Node, Types).

%% search_by_type(Node, Type) when Node#node.type =:= Type ->
%%     Node;
%% search_by_type(Node, Type) ->
%%     search_children(Node#node.children, Type).

%% search_children([], _Type) ->
%%     undefined;
%% search_children([Child|Rest], Type) ->
%%     case search_by_type(Child, Type) of
%%         undefined ->
%%             search_children(Rest, Type);
%%         Node ->
%%             Node
%%     end.
