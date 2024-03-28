-module(hm).
-compile(export_all).

-include("lint.hrl").

%% foo f g x = if f(x == 1) then g(x) else 20
simple() ->
    Equations = [{int, int},
                 {3, int},
                 {6, bool},
                 {1, {function, [], [6], 5}},
                 {2, {function, [], [3], 7}},
                 {5, bool},
                 {4, 7},
                 {4, int},
                 {0, {function, [], [1, 2, 3], 4}}],
    Node = #node{},
    Substitutions = unify_all_equations(Equations, Node, maps:new()),
    {function, [],
     [{function,[],[bool],bool},
      {function,[],[int], int},int],
     int} =
        dereference(Substitutions, 0).

all() ->
    simple(),
    Ignore = ["test_constructor.sa",
              "test_functions_adt.sa",
              "test_record2.sa",
              "test_type_alias.sa",
              "test_enum.sa",
              "test_record.sa"],
    case file:list_dir("../../../examples/sa") of
        {ok, Files} ->
            lists:foreach(
              fun("test_" ++ _ = Filename) ->
                      case lists:member(Filename, Ignore) of
                          true ->
                              ignore;
                          false ->
                              case filename:extension(Filename) of
                                  ".sa" ->
                                      FullPath =
                                          "../../../examples/sa/" ++ Filename,
                                      io:format("**** Testing ~s\n",
                                                [FullPath]),
                                      ok = start_test(FullPath);
                                  _ ->
                                      ignore
                              end
                      end;
                 (_) ->
                      ignore
              end, Files);
        Reason ->
            {error, Reason}
    end.

one(TestCase) ->
    FullPath = "../../../examples/sa/test_" ++ TestCase ++ ".sa",
    io:format("**** Testing ~s\n", [FullPath]),
    start_test(FullPath).

start_test(Filename) ->
    case lint:start(Filename) of
        {error, Reason} ->
            Reason;
        Result ->
            check_types(Result)
    end.

check_types({Source, Result, Node, AdornedEquations}) ->
    io:format("==== Source:\n~s\n", [Source]),
    io:format("==== Lint output:\n~s\n", [Result]),
    Equations =
        lists:map(fun(#equation{type = Type}) -> Type end, AdornedEquations),
    io:format("==== Equations:\n"),
    lists:foreach(
      fun(#equation{type = {Left, Right}}) ->
              io:format("~s = ~s\n",
                        [type_to_string(Left), type_to_string(Right)])
      end, AdornedEquations),
    io:format("\n"),
    case unify_all_equations(Equations, Node, maps:new()) of
        {mismatch, TypeStack} ->
            print_type_error(Source, Node, AdornedEquations, TypeStack),
            error;
        Substitutions ->
            io:format("==== Solutions:\n"),
            lists:foreach(
              fun(#equation{type = {Left, Right}}) ->
                      io:format("~s -> ~s\n",
                                [type_to_string(Left),
                                 type_to_string(
                                   dereference(Substitutions, Right))])
              end, AdornedEquations),
            ok
    end.

print_type_error(_Source, _Node, _AdornedEquations, TypeStack) ->
    %%io:format("**** Source:\n~s\n", [Source]),
    %%io:format("****Node:\n~p\n", [Node]),
    %%io:format("****Adorned equations:\n~p\n", [AdornedEquations]),
    %%io:format("****Type error:\n~p\n", [TypeStack]),
    io:format("==== Type error:\n~s", [format_type_stack(TypeStack)]).

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
unify(empty_list, {list, _Ys}, _TypeStack, _Node, Substitutions) ->
    Substitutions;
unify({list, _}, empty_list, _TypeStack, _Node, Substitutions) ->
    Substitutions;
unify({tuple, Xs}, {tuple, Ys}, TypeStack, Node, Substitutions) ->
    case length(Xs) /= length(Ys) of
        true ->
            {mismatch, TypeStack};
        false ->
            unify_args(Xs, Ys, TypeStack, Node, Substitutions)
    end;
unify(empty_map, {map, _KeyY, _ValueY}, _TypeStack, _Node,
      Substitutions) ->
    Substitutions;
unify({map, _KeyX, _ValueX}, empty_map, _TypeStack, _Node,
      Substitutions) ->
    Substitutions;
unify({map, KeyX, ValueX}, {map, KeyY, ValueY}, TypeStack, Node,
      Substitutions) ->
    unify_args([KeyX, ValueX], [KeyY, ValueY], TypeStack, Node, Substitutions);
unify({constructor, Xs}, {constructor, Ys}, TypeStack, Node, Substitutions) ->
    unify_args(Xs, Ys, TypeStack, Node, Substitutions);
unify({function, GenericsX, ArgsX, ReturnX},
      {function, GenericsY, ArgsY, ReturnY}, TypeStack, Node, Substitutions) ->
    if
        length(ArgsX) /= length(ArgsY) ->
            {mismatch, TypeStack};
        (GenericsX == [] andalso GenericsY == []) orelse
        (GenericsX == [] andalso GenericsY /= []) orelse
        (GenericsX /= [] andalso GenericsY == []) ->
            case unify_args(ArgsX, ArgsY, [{ArgsX, ArgsY}|TypeStack],
                            Node, Substitutions) of
                {mismatch, TypeStack} ->
                    {mismatch, TypeStack};
                ArgsSubstitutions ->
                    unify(ReturnX, ReturnY,
                          [{ReturnX, ReturnY}|TypeStack],
                          Node, ArgsSubstitutions)
            end;
        true ->
            case unify_args(GenericsX, GenericsY,
                            [{GenericsX, GenericsY}|TypeStack],
                            Node, Substitutions) of
                {mismatch, TypeStack} ->
                    {mismatch, TypeStack};
                GenericsSubstitutions ->
                    case unify_args(ArgsX, ArgsY, [{ArgsX, ArgsY}|TypeStack],
                                    Node, GenericsSubstitutions) of
                        {mismatch, TypeStack} ->
                            {mismatch, TypeStack};
                        ArgsSubstitutions ->
                            unify(ReturnX, ReturnY,
                                  [{ReturnX, ReturnY}|TypeStack],
                                  Node, ArgsSubstitutions)
                    end
            end
    end;
unify(_X, _Y, TypeStack, _Node, _Substitutions) ->
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
dereference(Substitutions, {function, _GenericTypes, ArgTypes, ReturnType}) ->
    {function, [],
     lists:map(fun(Type) -> dereference(Substitutions, Type) end, ArgTypes),
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
type_to_string(char) ->
    "Char";
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
type_to_string({function, [], [ArgType], ReturnType}) ->
    "(" ++ type_to_string(ArgType) ++ " -> " ++
        type_to_string(ReturnType) ++ ")";
type_to_string({function, [], ArgTypes, ReturnType}) ->
    "(["++ type_to_string(ArgTypes) ++ "] -> " ++
        type_to_string(ReturnType) ++ ")";
type_to_string({function, GenericTypes, [ArgType], ReturnType}) ->
    "(<" ++ type_to_string(GenericTypes) ++ ">" ++
        type_to_string(ArgType) ++ " -> " ++
        type_to_string(ReturnType) ++ ")";
type_to_string({function, GenericTypes, ArgTypes, ReturnType}) ->
    "(<" ++ type_to_string(GenericTypes) ++ ">" ++
        "["++ type_to_string(ArgTypes) ++ "] -> " ++
        type_to_string(ReturnType) ++ ")";
type_to_string(TypeVariable) when is_integer(TypeVariable) ->
    "t" ++ integer_to_list(TypeVariable);
type_to_string([]) ->
    "";
type_to_string([Type]) ->
    type_to_string(Type);
type_to_string([Type|Rest]) ->
    type_to_string(Type) ++ ", " ++ type_to_string(Rest).
