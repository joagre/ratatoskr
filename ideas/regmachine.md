# Machine instructions

## Jump instructions

JmpNotZeroR Rm
JmpNotZero


## Load instructions

## Integral arithmetic instructions

AddRR Rm Rn Ro
AddRI r0 r1 #
AddIR r0 # r1
SubRR r0 r1 r2
SubRI r0 r1 #
SubIR r0 # r1
MulRR r0 r1 r2
MulRI r0 r1 #
MulIR r0 # r1
DivRR r0 r1 r2
DivRI r0 r1 #
DivIR r0 # r1

## Floating point arithmetic instructions

The same instructions as listed in "Integral arithemetic instructions"
but for floating-point numbers, i.e. with "Float" pre-pended to the
instruction.





addrss Rm Sm Sm
  Add Sm and Sn and load result into Rm
addrrs Rm Rn Sm
  Add Rn and Sm and load result into Rm
addrrr Rm Rn Ro
  Add Rn and Ro and load result into Rm
addrsr Rm Sm Rn
  Add Sm and Rn and load result into Rm
addsss Sm Sn So
  Add Sn and So and load result into Sm
addsrs Sm Rm Sn
  Add Rm and Sn and load result into Sm
addsrr Sm Rm Rn
  Add Rm and Rn and load result into Sm
addssr Sm Sn Rm
  Add Sn and Rm and load result into Sm


## Stack instructions

PushValue
PushRegister
PushString
Pop
Dup
Swap
Load
Store
Add
Subtract
Multiply
Divide

## Call instructions

StackCall
StackModuleCall
StackReturn


    jmprnze,
    jmpringt,
    subrri,
    subrsi,
    addrri,
    loadri,
    pushr,
    loadrs,
    loadrr,
    rcall,
    rret,
    jmp,
    // Stack machine opcodes
    push,
    pushs,
    add,
    sub,
    mul,
    div,
    jump,
    cjump,
    call,
    ret,
    sys,
    and,
    or,
    not,
    eq,
    neq,
    lt,
    gt,
    nop,
    halt,
    mcall,
    spawn,
    mspawn



## Dynamic array instructions

DaCreate
DaPeek
DaPoke
DaGetLength
DaSetLength
DaAppendElementLast
DaAppendArrayLast
DaGetSlice
DaCopy
DaConcatenate

## Hash table instructions



append
