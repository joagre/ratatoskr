-module(hm).
-export([ex/0]).

%% foo f g x = if f(x == 1) then g(x) else 20
ex() ->
    Equations = [{int, int},
                 {3, int},
                 {6, bool},
                 {1, {[6], 5}},
                 {2, {[3], 7}},
                 {5, bool},
                 {4, 7},
                 {4, int},
                 {0, {[1, 2, 3], 4}}],
    Substitutions = unify_all_equations(Equations, maps:new()),
    {[{[bool],bool},{[int],int},int],int} =
        dereference(Substitutions, 0).

dereference(_Substitutions, BaseType) when is_atom(BaseType) ->
    BaseType;
dereference(Substitutions, Variable) when is_integer(Variable) ->
    dereference(Substitutions, maps:get(Variable, Substitutions));
dereference(Substitutions, {ArgTypes, ReturnType}) ->
    {lists:map(fun(Type) -> dereference(Substitutions, Type) end, ArgTypes),
     dereference(Substitutions, ReturnType)}.

unify_all_equations([], Substitutions) ->
    Substitutions;
unify_all_equations([{X, Y}|Rest], Substitutions) ->
    case unify(X, Y, Substitutions) of
        none ->
            none;
        UpdatedSubstitutions ->
            unify_all_equations(Rest, UpdatedSubstitutions)
    end.

unify(_X, _Y, none) ->
    none;
unify(X, X, Substitutions) ->
    Substitutions;
unify(X, Y, Substitutions) when is_integer(X) ->
    unify_variable(X, Y, Substitutions);
unify(X, Y, Substitutions) when is_integer(Y) ->
    unify_variable(Y, X, Substitutions);
unify({ArgsX, ReturnX}, {ArgsY, ReturnY}, Substitutions) ->
    case length(ArgsX) /= length(ArgsY) of
        true ->
            none;
        false ->
            unify_args(ArgsX, ArgsY, unify(ReturnX, ReturnY, Substitutions))
    end;
unify(_, _, _) ->
    none.

unify_args([], [], Substitutions) ->
    Substitutions;
unify_args([X|Xs], [Y|Ys], Substitutions) ->
    unify_args(Xs, Ys, unify(X, Y, Substitutions)).

unify_variable(Variable, Type, Substitutions) when is_integer(Variable) ->
    case maps:get(Variable, Substitutions, undefined) of
        undefined when is_integer(Type) ->
            case maps:get(Type, Substitutions, undefined) of
                undefined ->
                    case occurs_check(Variable, Type, Substitutions) of
                        true ->
                            none;
                        false ->
                            Substitutions#{Variable => Type}
                    end;
                Substitution ->
                    unify(Variable, Substitution, Substitutions)

            end;
        undefined ->
            case occurs_check(Variable, Type, Substitutions) of
                true ->
                    none;
                false ->
                    Substitutions#{Variable => Type}
            end;
        Substitution ->
            unify(Substitution, Type, Substitutions)
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
