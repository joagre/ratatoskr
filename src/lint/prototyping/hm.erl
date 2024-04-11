-module(hm).
-compile(export_all).

-include("lint.hrl").

%% foo f g x = if f(x == 1) then g(x) else 20
hello_world() ->
    Equations = [{int, int},
                 {{var, 3, none}, int},
                 {{var, 6, none}, bool},
                 {{var, 1, none},
                  {function, [], [{var, 6, none}], {var, 5, none}}},
                 {{var, 2, none},
                  {function, [], [{var, 3, none}], {var, 7, none}}},
                 {{var, 5, none}, bool},
                 {{var, 4, none}, {var, 7, none}},
                 {{var, 4, none}, int},
                 {{var, 0, none},
                  {function, [],
                   [{var, 1, none}, {var, 2, none}, {var, 3, none}],
                   {var, 4, none}}}],
    Substitutions = unify_all_equations(Equations, #node{}),
    {function, [],
     [{function,[],[bool],bool},
      {function,[],[int], int},int],
     int} =
        dereference(Substitutions, {var, 0, none}),
    ok.

test() ->
    ok = hello_world(),
    Ignore = ["test_constructor.sa",
              "test_functions_adt.sa",
              "test_record2.sa",
              "test_type_alias.sa",
              "test_enum.sa",
              "test_record.sa",
              "test_r.sa",
              "test_r2.sa",
              "test_stack.sa"],
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

test(TestCase) ->
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
    case instantiate_records(Equations) of
        {ok, InstantiatedEquations} ->
            case unify_all_equations(InstantiatedEquations, Node) of
                {mismatch, Why, TypeStack} ->
                    io:format("==== Why: ~w\n", [Why]),
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
            end;
        {error, Reason} ->
            {error, Reason}
    end.

instantiate_records(Equations) ->
    instantiate_records(Equations, Equations, []).

instantiate_records([], _Equations, Acc) ->
    {ok, lists:reverse(Acc)};
instantiate_records(
  [{Left,
    {record_instance, _Name, RecordDefType, GenericTypes, NamedArgs}}|Rest],
  Equations, Acc) ->
    case lookup_record_def(Equations, RecordDefType) of
        {ok, {record_def, Name, TypeVariables, MemberTypes}} ->
            instantiate_records(
              Rest, Equations,
              [{Left, {record, Name, GenericTypes, NamedArgs, TypeVariables,
                       MemberTypes}}|
               Acc]);
        {error, Reason} ->
            {error, Reason}
    end;
instantiate_records([{Left, Right}|Rest], Equations, Acc) ->
    instantiate_records(Rest, Equations, [{Left, Right}|Acc]).

lookup_record_def([], _RecordDefType) ->
    {error, "No record def"};
lookup_record_def(
  [{RecordDefType,
    {record_def, _Name, _TypeVariables, _MemberTypes} =
        RecordDefEquation}|_],
  RecordDefType) ->
    {ok, RecordDefEquation};
lookup_record_def([_|Rest], RecordDefType) ->
    lookup_record_def(Rest, RecordDefType).

print_type_error(_Source, _Node, _AdornedEquations, TypeStack) ->
    %%io:format("**** Source:\n~s\n", [Source]),
    %%io:format("****Node:\n~p\n", [Node]),
    %%io:format("****Adorned equations:\n~p\n", [AdornedEquations]),
    %%io:format("****Type error:\n~p\n", [TypeStack]),
    io:format("==== Type error:\n~s", [format_type_stack(TypeStack)]).

%%
%% Unify all equations
%%

unify_all_equations(Equations, Node) ->
    unify_all_equations(Equations, Equations, Node, #{deferred => []}).

unify_all_equations([], Equations, _Node, Substitutions) ->
    case maps:get(deferred, Substitutions, []) of
        [] ->
            Substitutions;
        Deferred ->
            io:format("==== Deferred: ~p\n", [Deferred]),
            unify_all_equations(Equations, Equations, _Node,
                                Substitutions#{deferred => []})
    end;
unify_all_equations([{Left, Right}|Rest], Equations, Node, Substitutions) ->
    case unify(Left, Right, [{Left, Right}], Node, Substitutions) of
        {mismatch, Why, TypeStack} ->
            {mismatch, Why, TypeStack};
        UpdatedSubstitutions ->
            unify_all_equations(Rest, Equations, Node, UpdatedSubstitutions)
    end.

unify(_X, _Y, _TypeStack, _Node, {mismatch, Why, TypeStack}) ->
    {mismatch, Why, TypeStack};
unify(X, X, _TypeStack, _Node, Substitutions) ->
    Substitutions;
unify({var, _Id, _Name} = X , Y, TypeStack, Node, Substitutions) ->
    unify_variable(X, Y, TypeStack, Node, Substitutions);
unify(X, {var, _Id, _Name} = Y, TypeStack, Node, Substitutions) ->
    unify_variable(Y, X, TypeStack, Node, Substitutions);
unify({list, X}, {list, Y}, TypeStack, Node, Substitutions) ->
    unify_all([X], [Y], TypeStack, Node, Substitutions);
unify(empty_list, {list, _Ys}, _TypeStack, _Node, Substitutions) ->
    Substitutions;
unify({list, _}, empty_list, _TypeStack, _Node, Substitutions) ->
    Substitutions;
unify({tuple, Xs}, {tuple, Ys}, TypeStack, Node, Substitutions) ->
    case length(Xs) /= length(Ys) of
        true ->
            {mismatch, tuple, TypeStack};
        false ->
            unify_all(Xs, Ys, TypeStack, Node, Substitutions)
    end;
unify(empty_map, {map, _KeyY, _ValueY}, _TypeStack, _Node,
      Substitutions) ->
    Substitutions;
unify({map, _KeyX, _ValueX}, empty_map, _TypeStack, _Node,
      Substitutions) ->
    Substitutions;
unify({map, KeyX, ValueX}, {map, KeyY, ValueY}, TypeStack, Node,
      Substitutions) ->
    unify_all([KeyX, ValueX], [KeyY, ValueY], TypeStack, Node, Substitutions);
unify({constructor, Xs}, {constructor, Ys}, TypeStack, Node, Substitutions) ->
    unify_all(Xs, Ys, TypeStack, Node, Substitutions);
unify({function, GenericsX, ArgsX, ReturnX},
      {function, GenericsY, ArgsY, ReturnY}, TypeStack, Node,
      Substitutions) ->
    case validate_function(GenericsX, ArgsX, GenericsY, ArgsY) of
        has_type_variables ->
            case unify_all(GenericsX, GenericsY,
                           [{GenericsX, GenericsY}|TypeStack], Node,
                           Substitutions) of
                {mismatch, Why, TypeStack} ->
                    {mismatch, Why, TypeStack};
                GenericsSubstitutions ->
                    case unify_all(ArgsX, ArgsY, [{ArgsX, ArgsY}|TypeStack],
                                   Node, GenericsSubstitutions) of
                        {mismatch, Why, TypeStack} ->
                            {mismatch, Why, TypeStack};
                        ArgsSubstitutions ->
                            unify(ReturnX, ReturnY,
                                  [{ReturnX, ReturnY}|TypeStack],
                                  Node, ArgsSubstitutions)
                    end
            end;
        has_no_type_variables ->
            case unify_all(ArgsX, ArgsY, [{ArgsX, ArgsY}|TypeStack],
                           Node, Substitutions) of
                {mismatch, Why, TypeStack} ->
                    {mismatch, Why, TypeStack};
                ArgsSubstitutions ->
                    unify(ReturnX, ReturnY,
                          [{ReturnX, ReturnY}|TypeStack],
                          Node, ArgsSubstitutions)
            end;
        {error, Why} ->
            {mismatch, Why, TypeStack}
    end;
unify({record, Name, GenericTypesX, NamedArgsX, TypeVariablesX, MemberTypesX},
      {record, Name, GenericTypesY, NamedArgsY, TypeVariablesY, MemberTypesY},
      TypeStack, Node, Substitutions)
  when length(GenericTypesX) == length(TypeVariablesX) andalso
       length(GenericTypesY) == length(TypeVariablesY) ->
    case verify_named_args(MemberTypesX, NamedArgsX) andalso
         verify_named_args(MemberTypesY, NamedArgsY) of
        false ->
            {mismatch, record, TypeStack};
        true ->
            case unify_all(GenericTypesX, TypeVariablesX, TypeStack, Node,
                            Substitutions) of
                {mismatch, Why, TypeStack} ->
                    {mismatch, Why, TypeStack};
                GenericSubstitutionsX ->
                    case unify_all(GenericTypesY, TypeVariablesY, TypeStack,
                                    Node, GenericSubstitutionsX) of
                        {mismatch, Why, TypeStack} ->
                            {mismatch, Why, TypeStack};
                        GenericSubstitutionsY ->
                            case unify_all(lists:sort(NamedArgsX),
                                            lists:sort(NamedArgsY), TypeStack,
                                            Node, GenericSubstitutionsY) of
                                {mismatch, Why, TypeStack} ->
                                    {mismatch, Why, TypeStack};
                                NamedArgsSubstitutions ->
                                    case unify_all(TypeVariablesX,
                                                    TypeVariablesY,
                                                    TypeStack, Node,
                                                    NamedArgsSubstitutions) of
                                        {mismatch, Why, TypeStack} ->
                                            {mismatch, Why, TypeStack};
                                        TypeVariablesSubstitutions ->
                                            unify_all(
                                              MemberTypesX, MemberTypesY,
                                              TypeStack, Node,
                                              TypeVariablesSubstitutions)
                                    end
                            end
                    end
            end
    end;
unify({record_dot, PostfixExprType, MemberName},
      Type, TypeStack, Node, #{deferred := Deferred} = Substitutions) ->
    case dereference(Substitutions, PostfixExprType) of
        {var, _, _} ->
            Substitutions#{deferred => [PostfixExprType|Deferred]};
        {record, _, _, NamedArgs, _, _} ->
            case lists:keyfind(MemberName, 2, NamedArgs) of
                {named_arg, _, MemberType} ->
                    unify(MemberType, Type, TypeStack, Node, Substitutions);
                false ->
                    {mismatch, record_dot, TypeStack}
            end
    end;
unify(Type, {record_dot, PostfixExprType, MemberName},
      TypeStack, Node, Substitutions) ->
    unify({record_dot, PostfixExprType, MemberName}, Type,
          TypeStack, Node, Substitutions);
unify({named_arg, Name, TypeX}, {named_arg, Name, TypeY},
      TypeStack, Node, Substitutions) ->
    unify(TypeX, TypeY, TypeStack, Node, Substitutions);
unify(X, Y, TypeStack, _Node, _Substitutions) ->
    io:format("**** X, Y: ~p != ~p\n", [X, Y]),
    {mismatch, generic, TypeStack}.

unify_all([], [], _TypeStack, _MetInfo, Substitutions) ->
    Substitutions;
unify_all([X|Xs], [Y|Ys], TypeStack, Node, Substitutions) ->
    unify_all(Xs, Ys, TypeStack, Node,
               unify(X, Y, [{X, Y}|TypeStack], Node, Substitutions)).

unify_variable(Variable, Type, TypeStack, Node, Substitutions) ->
    case {maps:get(Variable, Substitutions, undefined), Type} of
        {undefined, {var, _Id, _Name}} ->
            case maps:get(Type, Substitutions, undefined) of
                undefined ->
                    case occurs_check(Variable, Type, Substitutions) of
                        true ->
                            {mismatch, occurs_check, TypeStack};
                        false ->
                            Substitutions#{Variable => Type}
                    end;
                Substitution ->
                    unify(Variable, Substitution,
                          [{Variable, Substitution}|TypeStack], Node,
                          Substitutions)

            end;
        {undefined, _} ->
            case occurs_check(Variable, Type, Substitutions) of
                true ->
                    {mismatch, occurs_check, TypeStack};
                false ->
                    Substitutions#{Variable => Type}
            end;
        {Substitution, _} ->
            unify(Substitution, Type, [{Substitution, Type}|TypeStack],
                  Node, Substitutions)
    end.

occurs_check({var, Id, Name}, {var, Id, Name}, _Substitutions) ->
    true;
occurs_check({var, _, _} = Variable, {var, _, _} = Type, Substitutions) ->
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

validate_function(GenericsX, ArgsX, GenericsY, ArgsY) ->
    case {length(GenericsX), length(ArgsX), length(GenericsY), length(ArgsY)} of
        {_, M, _, N} when M /= N ->
            {error, function_args};
        {M, N, _, _} when M /= N, M /= 0 ->
            {error, function_left_generics};
        {_, _, M, N} when M /= N, M /= 0 ->
            {error, function_right_generics};
        {M, _, _, N} when M == N, M /= 0 ->
            has_type_variables;
        {0, _, _, _} ->
            has_no_type_variables;
        _ ->
            throw({internal_error, GenericsX, ArgsX, GenericsY, ArgsY})
    end.

verify_named_args(_MemberTypesX, _NamedArgsX) ->
    true.

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
dereference(Substitutions, {record_dot, X, MemberName}) ->
    {record_dot, dereference(Substitutions, X), MemberName};
dereference(Substitutions,
            {record, Name, GenericTypes, NamedArgs, TypeVariables,
             MemberTypes}) ->
    {record,
     Name,
     lists:map(fun(Type) ->
                       dereference(Substitutions, Type)
               end,
               GenericTypes),
     lists:map(fun({named_arg, ArgName, Type}) ->
                       {named_arg, ArgName, dereference(Substitutions, Type)}
               end,
               NamedArgs),
     lists:map(fun(Type) ->
                       dereference(Substitutions, Type)
               end,
               TypeVariables),
     lists:map(fun({property, MemberName, Modifier, Type}) ->
                       {property, MemberName, Modifier,
                        dereference(Substitutions, Type)};
                  ({method, MethodName, Modifier, MemberGenericTypes, ArgTypes,
                    ReturnType}) ->
                       {method, MethodName, Modifier,
                        lists:map(fun(Type) ->
                                          dereference(Substitutions, Type)
                                  end, MemberGenericTypes),
                        lists:map(fun(Type) ->
                                          dereference(Substitutions, Type)
                                  end, ArgTypes),
                        dereference(Substitutions, ReturnType)}
               end,
               MemberTypes)};
dereference(Substitutions, {record_instance, Name, RecordDefType, GenericTypes,
                            NamedArgs}) ->
    {record_instance, Name, dereference(Substitutions, RecordDefType),
     lists:map(fun(Type) -> dereference(Substitutions, Type) end, GenericTypes),
     lists:map(fun({named_arg, ArgName, Type}) ->
                       {named_arg, ArgName,
                        dereference(Substitutions, Type)} end,
               NamedArgs)};
dereference(Substitutions, {record_def, Name, TypeVariables, MemberTypes}) ->
    {record_def, Name,
     lists:map(fun(TypeVariable) ->
                       dereference(Substitutions, TypeVariable) end,
               TypeVariables),
     lists:map(fun({property, MemberName, AccessModifier, Type}) ->
                       {property, MemberName, AccessModifier,
                        dereference(Substitutions, Type)};
                  ({method, MethodName, AccessModifier, GenericTypes, ArgTypes,
                    ReturnType}) ->
                       {method, MethodName, AccessModifier,
                        lists:map(fun(Type) ->
                                          dereference(Substitutions, Type)
                                  end, GenericTypes),
                        lists:map(fun(Type) ->
                                          dereference(Substitutions, Type)
                                  end, ArgTypes),
                        dereference(Substitutions, ReturnType)}
               end, MemberTypes)};
dereference(Substitutions, {var, _, _} = Variable) ->
    case maps:get(Variable, Substitutions, undefined) of
        undefined ->
            Variable;
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
type_to_string({function, [], ArgTypes, ReturnType}) ->
    "(fn(" ++ type_to_string(ArgTypes) ++ ") -> " ++
        type_to_string(ReturnType) ++ ")";
type_to_string({function, GenericTypes, ArgTypes, ReturnType}) ->
    "(fn<" ++ type_to_string(GenericTypes) ++ ">" ++
        "("++ type_to_string(ArgTypes) ++ ") -> " ++
        type_to_string(ReturnType) ++ ")";
type_to_string({record_def, Name, [], MemberTypes}) ->
    "(record " ++ Name ++ " { " ++ type_to_string(MemberTypes) ++ " })";
type_to_string({record_def, Name, TypeVariables, MemberTypes}) ->
    "(record " ++ Name ++ "<" ++ type_to_string(TypeVariables) ++ "> { " ++
        type_to_string(MemberTypes) ++ " })";
type_to_string({property, MemberName, Modifier, Type}) ->
    modifier_to_string(Modifier) ++ " " ++
        MemberName ++ ": " ++ type_to_string(Type);
type_to_string({method, Name, Modifier, GenericTypes, ArgTypes, ReturnType}) ->
    modifier_to_string(Modifier) ++ " fn " ++ Name ++ "<" ++
        type_to_string(GenericTypes) ++ ">(" ++ type_to_string(ArgTypes) ++
        ") -> " ++ type_to_string(ReturnType);
type_to_string({record_instance, _Name, RecordDefType, [], []}) ->
    type_to_string(RecordDefType) ++ "(:)";
type_to_string({record_instance, _Name, RecordDefType, GenericTypes, []}) ->
    type_to_string(RecordDefType) ++
        "<" ++ type_to_string(GenericTypes) ++ ">(:)";
type_to_string({record_instance, _Name, RecordDefType, [], NamedArgs}) ->
    type_to_string(RecordDefType) ++ " (" ++ type_to_string(NamedArgs) ++ ")";
type_to_string({record_instance, _Name, RecordDefType, GenericTypes,
                NamedArgs}) ->
    type_to_string(RecordDefType) ++ "<" ++
        type_to_string(GenericTypes) ++ ">(" ++ type_to_string(NamedArgs) ++
        ")";
type_to_string({record, Name, GenericTypes, NamedArgs, _TypeVariables,
                MemberTypes}) ->
    "(record " ++ Name ++ "<" ++ type_to_string(GenericTypes) ++ "> " ++
        type_to_string(NamedArgs) ++ " { " ++ type_to_string(MemberTypes) ++
        " })";
type_to_string({named_arg, Name, Type}) ->
    Name ++ ": " ++ type_to_string(Type);
type_to_string({record_dot, PostfixExprType, MemberName}) ->
    type_to_string(PostfixExprType) ++ "." ++ MemberName;
type_to_string({var, Id, none}) ->
    "t" ++ integer_to_list(Id);
type_to_string({var, Id, Name}) ->
    "t" ++ integer_to_list(Id) ++ ":" ++ Name;
type_to_string([]) ->
    "";
type_to_string([Type]) ->
    type_to_string(Type);
type_to_string([Type|Rest]) ->
    type_to_string(Type) ++ ", " ++ type_to_string(Rest).

modifier_to_string(private) -> "private";
modifier_to_string(private_const) -> "private";
modifier_to_string(public) -> "public";
modifier_to_string(public_const) -> "public constant";
modifier_to_string(readonly) -> "readonly".
