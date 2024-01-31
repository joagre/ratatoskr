-module(hm).
-export([ex/0, hm1/0]).

%% foo f g x = if f(x == 1) then g(x) else 20
ex() ->
    Equations = [{int, int},
                 {t3, int},
                 {t6, bool},
                 {t1, {[t6], t5}},
                 {t2, {[t3], t7}},
                 {t5, bool},
                 {t4, t7},
                 {t4, int},
                 {t0, {[t1, t2, t3], t4}}],
    Substitutions = unify_all_equations(Equations, maps:new()),
    {[{[bool],bool},{[int],int},int],int} =
        dereference(Substitutions, 0).

hm1() ->
    CEquations = parse_equations("../../../examples/sa/hm1.sa"),
    Equations =
        lists:map(fun({_, _, _, _, _, Equation}) ->
                          Equation end,
                  CEquations),
    Substitutions = unify_all_equations(Equations, maps:new()),
    dereference(Substitutions, 0).

parse_equations(Filename) ->
    parse_equation_lines(
      string:split(os:cmd("../../../bin/lint < " ++ Filename), "\n",all)).

parse_equation_lines([]) ->
    [];
parse_equation_lines([""|Rest]) ->
    parse_equation_lines(Rest);
parse_equation_lines([Line|Rest]) ->
    [OriginNode, OriginRow, Row, Info, Value, Type] =
        string:split(Line, ":", all),
    [{OriginNode, OriginRow, Row, Info, Value, parse_type(string:trim(Type))}|
     parse_equation_lines(Rest)].

parse_type(Line) ->
    {ok, Tokens, _} = erl_scan:string(Line ++ "."),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    [Expr] = Parsed,
    {value, Result, _} = erl_eval:exprs([Expr], erl_eval:new_bindings()),
    Result.

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
