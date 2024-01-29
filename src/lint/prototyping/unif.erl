-module(unif).
-export([ex1/0, ex2/0]).

-record(entry,
        {
         term,
         functor,
         type,
         arity = 0,
         components = []
        }).

ex1() ->
    X = "p(Z,h(Z,W),f(W))",
    Y = "p(f(X),h(Y,f(a)),Y)",
    ParseTree =
        [{"p(Z,h(Z,W),f(W)",
          [{"Z", []},
           {"h(Z,W)",
            [{"Z", []},
             {"W", []}]},
           {"f(W)",
            [{"W", []}]}]},
         {"p(f(X),h(Y,f(a)),Y)",
          [{"f(X)",
            [{"X", []}]},
           {"h(Y,f(a))",
            [{"Y", []},
             {"f(a)",
              [{"a", []}]}]},
           {"Y", []}]}],
    unify(ParseTree, X, Y, 11, 6).

ex2() ->
    X = "f(X1,g(X2,X3),X2,b)",
    Y = "f(g(h(a,X5),X2),X1,h(a,X4),X4)",
    ParseTree =
        [{"f(X1,g(X2, X3),X2,b)",
          [{"X1", []},
           {"g(X2,X3)",
            [{"X2", []},
             {"X3", []}]},
           {"X2", []},
           {"b", []}]},
         {"f(g(h(a,X5),X2),X1,h(a,X4),X4)",
          [{"g(h(a,X5),X2)",
            [{"h(a,X5)",
              [{"a", []},
               {"X5", []}]},
             {"X2", []}]},
           {"X1", []},
           {"h(a,X4)",
            [{"a", []},
             {"X4", []}]},
           {"X4", []}]}],
    unify(ParseTree, X, Y, 13, 9).

unify(ParseTree, X, Y, Ix, Iy) ->
    %% Step0: Print X and Y
    io:format("Problem: ~s = ~s\n", [X, Y]),
    %% Step1: Build unification table
    {UT, _, _} = build_unification_table(ParseTree, maps:new(), 0),
    io:format("\nUnification Table:\n"),
    io:format("\n~-33s ~-6s ~-8s ~-10s ~-6s ~-11s\n",
              ["Term", "Index", "Functor", "Type", "Arity", "Components"]),
    io:format("~-33s ~-6s ~-8s ~-10s ~-6s ~-11s\n",
              ["----", "-----", "------", "----", "-----", "-----------"]),
    lists:foreach(
      fun({Index, #entry{term = Term,
                         functor = Functor,
                         type = Type,
                         arity = Arity,
                         components = Components}}) ->
              io:format("~-33s ~-6w ~-8s ~-10w ~-6w ~-11w\n",
                        [Term, Index, Functor, Type, Arity, Components])
      end, lists:keysort(1, maps:to_list(UT))),
    io:format("\nSolution:\n\n"),
    %% Step2: Bind variables
    Bindings = bind_variables(UT, Ix, Iy),
    Solution =
        maps:fold(
          fun(Ix2, Iy2, Acc) ->
                  #entry{term = XTerm} = lookup_entry(UT, Ix2),
                  DIy2 = dereference_variable(Bindings, Iy2),
                  [{XTerm, termify_components(UT, Bindings, DIy2)}|Acc]
          end, [], Bindings),
    lists:foreach(
      fun({Variable, Term}) ->
              io:format("~s = ~s\n", [Variable, Term])
      end, lists:keysort(1, Solution)).

%% Build unification table

build_unification_table([], UT, Index) ->
    {UT, Index, []};
build_unification_table([TreeNode|Rest], UT, Index) ->
    {RestUT, NextIndex, _} = build_unification_table(Rest, UT, Index),
    case TreeNode of
        %% Composite
        {[FunctionName, $(|_] = Term, ChildrenTreeNodes} ->
            {NewUT, IndexAfterComponents, ComponentIndices} =
                get_component_indices(RestUT, NextIndex,
                                      lists:reverse(ChildrenTreeNodes)),
            Entry = #entry{term = Term,
                           functor = [FunctionName],
                           type = str,
                           arity = length(ComponentIndices),
                           components = ComponentIndices},
            FinalUT = insert_entry(NewUT, IndexAfterComponents, Entry),
            {FinalUT, IndexAfterComponents + 1, IndexAfterComponents};
        %% Variable
        {[C|_] = Term, []} when C < $a ->
            case find_term_index(RestUT, Term) of
                not_found ->
                    Entry = #entry{term = Term,
                                   functor = Term,
                                   type = var},
                    NewUT = insert_entry(RestUT, NextIndex, Entry),
                    {NewUT, NextIndex + 1, NextIndex};
                ExistingIndex ->
                    {RestUT, NextIndex, ExistingIndex}
            end;
        %% Constant
        {[C|_] = Term, []} when C >= $a ->
            Entry = #entry{term = Term,
                           functor = Term,
                           type = str},
            NewUT = insert_entry(RestUT, NextIndex, Entry),
            {NewUT, NextIndex + 1, NextIndex}
    end.

get_component_indices(UT, NextIndex, TreeNodes) ->
    get_component_indices(UT, NextIndex, TreeNodes, []).

get_component_indices(UT, NextIndex, [], ComponentIndices) ->
    {UT, NextIndex, ComponentIndices};
get_component_indices(UT, NextIndex, [TreeNode|Rest], ComponentIndices) ->
    {NewUT, NextNextIndex, ComponentIndex} =
        build_unification_table([TreeNode], UT, NextIndex),
    get_component_indices(NewUT, NextNextIndex, Rest,
                          [ComponentIndex|ComponentIndices]).

%% Bind variables

bind_variables(UT, X, Y) ->
    Sx = push(X, new_stack()),
    Sy = push(Y, new_stack()),
    bind_variables(UT, Sx, Sy, maps:new()).

bind_variables(_UT, [], [], Bindings) ->
    Bindings;
bind_variables(UT, Sx, Sy, Bindings) ->
    {Ix, #entry{functor = XFunctor, type = XType, arity = XArity,
                components = XComponents} = XEntry, PoppedSx} =
        pop(UT, Sx),
    {Iy, #entry{functor = YFunctor, type = YType, arity = YArity,
               components = YComponents} = YEntry, PoppedSy} =
        pop(UT, Sy),
    if
        %% Case 1: x is bound to a term and y is bound to a term
        XType == str andalso YType == str ->
            if
                XFunctor == YFunctor andalso XArity == YArity ->
                    if
                        XArity > 0 ->
                            PushedSx = push(XComponents, PoppedSx),
                            PushedSy = push(YComponents, PoppedSy),
                            bind_variables(UT, PushedSx, PushedSy, Bindings);
                        true ->
                            bind_variables(UT, PoppedSx, PoppedSy, Bindings)
                    end;
                true ->
                    throw({mismatch, XEntry, YEntry})
            end;
        %% Case 2: x is bound to a term and y is bound to a variable
        XType == str andalso YType == var ->
            case is_free_variable(Bindings, Iy) of
                true ->
                    NewBindings = bind_variable(Bindings, Iy, Ix),
                    bind_variables(UT, PoppedSx, PoppedSy, NewBindings);
                false-> % y is bound
                    DIy = dereference_variable(Bindings, Iy),
                    #entry{type = DYType} = lookup_entry(UT, DIy),
                    if
                        DYType == str ->
                            PushedSx = push(Ix, PoppedSx),
                            PushedSy = push(DIy, PoppedSy),
                            bind_variables(UT, PushedSx, PushedSy, Bindings);
                        true -> % y is bound to a free variable
                            NewBindings = bind_variable(Bindings, DIy, Ix),
                            bind_variables(UT, PoppedSx, PoppedSy, NewBindings)
                    end
            end;
        %% Case 3: x is bound to a variable and y is bound to a term
        XType == var andalso YType == str ->
            case is_free_variable(Bindings, Ix) of
                true ->
                    NewBindings = bind_variable(Bindings, Ix, Iy),
                    bind_variables(UT, PoppedSx, PoppedSy, NewBindings);
                false -> % x is bound
                    DIx = dereference_variable(Bindings, Ix),
                    #entry{type = DXType} = lookup_entry(UT, DIx),
                    if
                        DXType == str ->
                            PushedSx = push(DIx, PoppedSx),
                            PushedSy = push(Iy, PoppedSy),
                            bind_variables(UT, PushedSx, PushedSy, Bindings);
                        true -> % x is bound to a free variable
                            NewBindings = bind_variable(Bindings, DIx, Iy),
                            bind_variables(UT, PoppedSx, PoppedSy, NewBindings)
                    end
            end;
        %% Case 4: x is bound to a variable and y is bound to a variable
        XType == var andalso YType == var ->
            XFree = is_free_variable(Bindings, Ix),
            YFree = is_free_variable(Bindings, Iy),
            if
                XFree andalso YFree ->
                    NewBindings = bind_variable(Bindings, Ix, Iy),
                    bind_variables(UT, PoppedSx, PoppedSy, NewBindings);
                XFree andalso not YFree ->
                    NewBindings = bind_variable(Bindings, Ix, Iy),
                    bind_variables(UT, PoppedSx, PoppedSy, NewBindings);
                not XFree andalso YFree ->
                    NewBindings = bind_variable(Bindings, Iy, Ix),
                    bind_variables(UT, PoppedSx, PoppedSy, NewBindings);
                not XFree andalso not YFree ->
                    DIx = dereference_variable(Bindings, Ix),
                    PushedSx = push(DIx, PoppedSx),
                    DIy = dereference_variable(Bindings, Iy),
                    PushedSy = push(DIy, PoppedSy),
                    bind_variables(UT, PushedSx, PushedSy, Bindings)
            end
    end.

%% Bindings

is_free_variable(Bindings, I) -> not(maps:is_key(I, Bindings)).

bind_variable(Bindings, I, OtherI) -> maps:put(I, OtherI, Bindings).

dereference_variable(Bindings, I) ->
    case maps:get(I, Bindings, unknown_index) of
        unknown_index -> I;
        DI -> dereference_variable(Bindings, DI)
    end.

%% Unification table

lookup_entry(UT, I) -> maps:get(I, UT).

insert_entry(UT, I, Entry) -> maps:put(I, Entry, UT).

find_term_index(UT, Term) ->
    Iterator = maps:iterator(UT),
    lookup_term_index(Iterator, Term).

lookup_term_index(Iterator, Term) ->
    case maps:next(Iterator) of
        none -> not_found;
        {I, #entry{term = Term}, _} -> I;
        {_I, _Entry, NextIterator} -> lookup_term_index(NextIterator, Term)
    end.

termify_components(UT, Bindings, DI) ->
    case lookup_entry(UT, DI) of
        #entry{functor = Functor, arity = 0} -> Functor;
        #entry{functor = Functor, components = Components} ->
            TermifiedComponentIndices =
                termify_component_indices(UT, Bindings, Components),
            Functor ++ "(" ++
                lists:flatten(add_commas(TermifiedComponentIndices)) ++ ")"
    end.

termify_component_indices(_UT, _Bindings, []) -> [];
termify_component_indices(UT, Bindings, [I|Rest]) ->
    DI = dereference_variable(Bindings, I),
    case lookup_entry(UT, DI) of
        #entry{functor = Functor, type = str, arity = 0} ->
            [Functor|termify_component_indices(UT, Bindings, Rest)];
        _ ->
            [termify_components(UT, Bindings, DI)|
             termify_component_indices(UT, Bindings, Rest)]
    end.

add_commas([]) -> "";
add_commas([S]) -> S;
add_commas([S|Rest]) -> S ++ "," ++ add_commas(Rest).

%% Stack

new_stack() -> [].

push([], Stack) -> Stack;
push([I|Rest], Stack) -> push(Rest, push(I, Stack));
push(I, Stack) -> [I|Stack].

pop(UT, [I|Stack]) ->
    Entry = lookup_entry(UT, I),
    {I, Entry, Stack}.
