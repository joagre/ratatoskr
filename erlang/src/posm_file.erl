-module(posm_file).
-export([read_file/1]).

-include("posm.hrl").

read_file(Filename) ->
    case file:open(Filename, [read]) of
        {ok, Fd} ->
            remove_labels(parse_file(Fd));
        {error, Reason} ->
            {error, Reason}
    end.

parse_file(Fd) ->
    case file:read_line(Fd) of
        eof ->
            [];
        {ok, Line} ->
            case string:trim(normalize(Line)) of
                "" ->
                    parse_file(Fd);
                CanonicalLine ->
                    [parse_instruction(CanonicalLine)|parse_file(Fd)]
            end;
        {error, Reason} ->
            throw({file, Reason})
    end.

normalize([]) -> [];
normalize([$;|_]) -> [];
normalize([$\t|Rest]) -> normalize([$ |Rest]);
normalize([$ , $\t|Rest]) -> normalize([$ |Rest]);
normalize([$ , $ |Rest]) -> normalize([$ |Rest]);
normalize([C|Rest]) -> [C|normalize(Rest)].

parse_instruction("LABEL " ++ Label) ->
    {label, list_to_integer(Label)};
parse_instruction("PUSH " ++ Value) ->
    {push, list_to_integer(Value)};
parse_instruction("PUSHR " ++ Register) ->
    {pushr, which_register(Register)};
parse_instruction("POP") ->
    pop;
parse_instruction("DUP") ->
    dup;
parse_instruction("SWAP") ->
    swap;
parse_instruction("LOADR " ++ Register) ->
    {loadr, which_register(Register, [sp, fp])};
parse_instruction("STORER " ++ Register) ->
    {storer, which_register(Register, [sp, fp])};
parse_instruction("MOVER " ++ Register) ->
    {mover, which_register(Register)};
parse_instruction("ADD") ->
    add;
parse_instruction("SUB") ->
    sub;
parse_instruction("MUL") ->
    mul;
parse_instruction("DIV") ->
    'div';
parse_instruction("JUMP" ++ Label) ->
    {jump, list_to_integer(string:trim(Label))};
parse_instruction("CJUMP" ++ Label) ->
    {cjump, list_to_integer(string:trim(Label))};
parse_instruction("CALL" ++ Label) ->
    {call, list_to_integer(string:trim(Label))};
parse_instruction("RET") ->
    ret;
parse_instruction("SYS " ++ Name) ->
    {sys, which_sys_name(string:trim(Name))};
parse_instruction("AND") ->
    'and';
parse_instruction("OR") ->
    'or';
parse_instruction("NOT") ->
    'not';
parse_instruction("EQ") ->
    eq;
parse_instruction("NEQ") ->
    neq;
parse_instruction("LT") ->
    lt;
parse_instruction("GT") ->
    gt;
parse_instruction("NOP") ->
    nop;
parse_instruction("HALT") ->
    halt;
parse_instruction(UnknownInstruction) ->
    throw({bad_instruction, UnknownInstruction}).

which_register(String, ValidRegisters) ->
    Register = which_register(String),
    case lists:member(Register, ValidRegisters) of
        true ->
            Register;
        false ->
            throw({invalid_register, Register})
    end.

which_register("SP") -> sp;
which_register("FP") -> fp;
which_register("PC") -> sp;
which_register(Register) -> throw({unknown_register, Register}).

which_sys_name("spawn") -> ?SYS_SPAWN;
which_sys_name("send") -> ?SYS_SEND;
which_sys_name("recv") -> ?SYS_RECV;
which_sys_name("rand") -> ?SYS_RAND;
which_sys_name("sleep") -> ?SYS_SLEEP;
which_sys_name("println") -> ?SYS_PRINTLN;
which_sys_name(Name) -> throw({unknown_sys_name, Name}).

remove_labels(Instructions) ->
    remove_labels(Instructions, {[], #{}, 0}).

remove_labels([], {Program, JumpTable, _Address}) ->
    {array:from_list(patch_labels(JumpTable, lists:reverse(Program))),
     JumpTable};
remove_labels([{label, Label}|Rest], {Program, JumpTable, Address}) ->
    remove_labels(Rest, {Program, JumpTable#{Label => Address}, Address});
remove_labels([Instruction|Rest], {Program, JumpTable, Address}) ->
    remove_labels(Rest, {[Instruction|Program], JumpTable, Address + 1}).

patch_labels(_JumpTable, []) ->
    [];
patch_labels(JumpTable, [{Name, Label}|Rest])
  when Name == jump orelse
       Name == cjump orelse
       Name == call ->
    Address = maps:get(Label, JumpTable),
    [{Name, Address}|patch_labels(JumpTable, Rest)];
patch_labels(JumpTable, [Instruction|Rest]) ->
    [Instruction|patch_labels(JumpTable, Rest)].
