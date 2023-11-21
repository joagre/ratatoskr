-module(posm).
-export([start/1, start/2]).

-include("posm.hrl").

-define(DEBUG(Format, Args), io:format(Format ++ "\n", Args)).
%%-define(DEBUG(Format, Args), ok).

start([Filename, StartLabel]) ->
    start(Filename, list_to_integer(StartLabel)),
    erlang:halt().

start(Filename, StartLabel) ->
    {Program, JumpTable} = posm_file:read_file(Filename),
    Stack = array:new(),
    StartAddress = maps:get(StartLabel, JumpTable),
    Registers = #{sp => -1, fp => 0, pc => StartAddress},
    ?DEBUG("~p", [format_program(Program)]),
    process_instructions(Program, Stack, Registers).

process_instructions(Program, Stack, #{sp := SP, pc := PC} = Registers) ->
    Instruction = array:get(PC, Program),
    ?DEBUG("~p", [format_stack(Stack, SP)]),
    ?DEBUG("~p: ~p", [PC, Instruction]),
    case eval(Stack, Registers, Instruction) of
        halt ->
            io:format("~p\n", [format_stack(Stack, SP)]);
        {UpdatedStack, #{pc := PC} = UpdatedRegisters} ->
            process_instructions(Program, UpdatedStack,
                                 UpdatedRegisters#{pc => PC + 1});
        {UpdatedStack, UpdatedRegisters} ->
            process_instructions(Program, UpdatedStack, UpdatedRegisters)
    end.

eval(Stack, #{sp := SP} = Registers, {push, Value}) ->
    UpdatedSP = SP + 1,
    UpdatedStack = array:set(UpdatedSP, Value, Stack),
    {UpdatedStack, Registers#{sp => UpdatedSP}};
eval(Stack, #{sp := SP} = Registers, pop) ->
    UpdatedSP = SP - 1,
    {Stack, Registers#{sp => UpdatedSP}};
eval(Stack, #{sp := SP} = Registers, dup) ->
    TopValue = top1(Stack, SP),
    UpdatedSP = SP + 1,
    UpdatedStack = array:set(UpdatedSP, TopValue, Stack),
    {UpdatedStack, Registers#{sp => UpdatedSP}};
eval(Stack, #{sp := SP} = Registers, swap) ->
    {TopValue, NextToTopValue} = top2(Stack, SP),
    UpdatedSP = SP - 1,
    UpdatedStack =
        array:set(SP, NextToTopValue,
                  array:set(UpdatedSP, TopValue, Stack)),
    {UpdatedStack, Registers};
eval(Stack, #{sp := SP} = Registers, {load, sp}) ->
    Offset = top1(Stack, SP),
    Value = array:get(SP - Offset, Stack),
    UpdatedStack = array:set(SP, Value, Stack),
    {UpdatedStack, Registers};
eval(Stack, #{sp := SP, fp := FP} = Registers, {load, fp}) ->
    Offset = top1(Stack, SP),
    Value = array:get(FP - Offset, Stack),
    UpdatedStack = array:set(SP, Value, Stack),
    {UpdatedStack, Registers};
eval(Stack, #{sp := SP} = Registers, {store, sp}) ->
    {Offset, NewValue} = top2(Stack, SP),
    UpdatedSP = SP - 2,
    %% Do not count the STORER parameters
    UpdatedStack = array:set(UpdatedSP - Offset, NewValue, Stack),
    {UpdatedStack, Registers#{sp => UpdatedSP}};
eval(Stack, #{sp := SP, fp := FP} = Registers, {store, fp}) ->
    {Offset, NewValue} = top2(Stack, SP),
    UpdatedStack = array:set(FP - Offset, NewValue, Stack),
    UpdatedSP = SP - 2,
    {UpdatedStack, Registers#{sp => UpdatedSP}};
eval(Stack, #{sp := SP} = Registers, add) ->
    {Operand2, Operand1} = top2(Stack, SP),
    UpdatedSP = SP - 1,
    UpdatedStack = array:set(UpdatedSP, Operand1 + Operand2, Stack),
    {UpdatedStack, Registers#{sp => UpdatedSP}};
eval(Stack, #{sp := SP} = Registers, sub) ->
    {Operand2, Operand1} = top2(Stack, SP),
    UpdatedSP = SP - 1,
    UpdatedStack = array:set(UpdatedSP, Operand1 - Operand2, Stack),
    {UpdatedStack, Registers#{sp => UpdatedSP}};
eval(Stack, #{sp := SP} = Registers, mul) ->
    {Operand2, Operand1} = top2(Stack, SP),
    UpdatedStack = array:set(SP - 1, trunc(Operand1 * Operand2), Stack),
    {UpdatedStack, Registers#{sp => SP - 1}};
eval(Stack, #{sp := SP} = Registers, 'div') ->
    {Operand2, Operand1} = top2(Stack, SP),
    UpdatedStack = array:set(SP - 1, trunc(Operand1 / Operand2), Stack),
    {UpdatedStack, Registers#{sp => SP - 1}};
eval(Stack, Registers, {jump, Address}) ->
    {Stack, Registers#{pc => Address}};
eval(Stack, #{sp := SP} = Registers, {cjump, Address}) ->
    UpdatedSP = SP - 1,
    case to_bool(top1(Stack, SP)) of
        true ->
            {Stack, Registers#{sp => UpdatedSP, pc => Address}};
        false ->
            {Stack, Registers#{sp => UpdatedSP}}
    end;
eval(Stack, #{sp := SP, pc := PC} = Registers, {call, Address, Arity}) ->
    UpdatedSP = SP + 1,
    ReturnAddress = PC + 1,
    UpdatedStack = array:set(UpdatedSP, FP,
                             array:set(UpdatedSP, ReturnAddress, Stack)),
    {UpdatedStack, Registers#{sp => UpdatedSP,
                              fp => SP - 2 - Arity,
                              pc => Address}};
eval(Stack, #{sp := SP} = Registers, ret) ->
    {ReturnAddress, Result, NumberOfParameters} = top3(Stack, SP),
    UpdatedSP = SP - 2 - NumberOfParameters,
    UpdatedStack = array:set(UpdatedSP, Result, Stack),
    {UpdatedStack, Registers#{sp => UpdatedSP, pc => ReturnAddress}};
eval(_Stack, _Registers, {sys, _Name}) ->
    throw({not_implemented, sys});
eval(Stack, #{sp := SP} = Registers, 'and') ->
    {TopValue, NextToTopValue} = top2(Stack, SP),
    UpdatedStack =
        array:set(SP - 1,
                  from_bool(to_bool(NextToTopValue) and to_bool(TopValue)),
                  Stack),
    {UpdatedStack, Registers#{sp => SP - 1}};
eval(Stack, #{sp := SP} = Registers, 'or') ->
    UpdatedSP = SP - 1,
    {Operand2, Operand1} = top2(Stack, SP),
    UpdatedStack =
        array:set(Stack, UpdatedSP,
                  from_bool(to_bool(Operand1) or to_bool(Operand2))),
    {UpdatedStack, Registers#{sp => UpdatedSP}};
eval(Stack, #{sp := SP} = Registers, 'not') ->
    TopValue = top1(Stack, SP),
    UpdatedStack =
        array:set(SP, from_bool(not(to_bool(TopValue))), Stack),
    {UpdatedStack, Registers#{sp => SP}};
eval(Stack, #{sp := SP} = Registers, eq) ->
    UpdatedSP = SP - 1,
    {Operand2, Operand1} = top2(Stack, SP),
    UpdatedStack = array:set(UpdatedSP, from_bool(Operand1 == Operand2), Stack),
    {UpdatedStack, Registers#{sp => UpdatedSP}};
eval(Stack, #{sp := SP} = Registers, neq) ->
    UpdatedSP = SP - 1,
    {Operand2, Operand1} = top2(Stack, SP),
    UpdatedStack = array:set(UpdatedSP, from_bool(Operand1 /= Operand2), Stack),
    {UpdatedStack, Registers#{sp => UpdatedSP}};
eval(Stack, #{sp := SP} = Registers, lt) ->
    UpdatedSP = SP - 1,
    {Operand2, Operand1} = top2(Stack, SP),
    UpdatedStack = array:set(UpdatedSP, from_bool(Operand1 < Operand2), Stack),
    {UpdatedStack, Registers#{sp => UpdatedSP}};
eval(Stack, #{sp := SP} = Registers, gt) ->
    UpdatedSP = SP - 1,
    {Operand2, Operand1} = top2(Stack, SP),
    UpdatedStack = array:set(UpdatedSP, from_bool(Operand1 > Operand2), Stack),
    {UpdatedStack, Registers#{sp => UpdatedSP}};
eval(Stack, Registers, nop) ->
    {Stack, Registers};
eval(_Stack, _Registers, halt) ->
    halt.

top1(Stack, SP) ->
    array:get(SP, Stack).

top2(Stack, SP) ->
    {array:get(SP, Stack), array:get(SP - 1, Stack)}.

top3(Stack, SP) ->
    {array:get(SP, Stack), array:get(SP - 1, Stack), array:get(SP - 2, Stack)}.

to_bool(0) -> false;
to_bool(_) -> true.

from_bool(false) -> 0;
from_bool(true) -> 1.

format_stack(Stack, SP) ->
    format_stack_list(array:to_list(Stack), SP).

format_stack_list(List, -1) ->
    List;
format_stack_list(List, SP) ->
    lists:sublist(List, 1, SP + 1).

format_program(Program) ->
    format_program(array:to_list(Program), 0).

format_program([], _Address) ->
    [];
format_program([Instruction|Rest], Address) ->
    [{Address, Instruction}|format_program(Rest, Address + 1)].
