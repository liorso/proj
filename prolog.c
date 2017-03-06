#include <stdio.h>
#include <stdlib.h>
#define DO_SHOW 1

#include "arch/cisc.h"
#include "arch/debug_macros.h"

int main()
{
START_MACHINE;

JUMP(CONTINUE);
#include "arch/char.lib"
#include "arch/io.lib"
#include "arch/math.lib"
#include "arch/string.lib"
#include "arch/system.lib"
#include "arch/scheme.lib"

CONTINUE:
PUSH(0);
PUSH(0);
PUSH(0);
PUSH(0);
MOV(FP, SP);
MOV(R16, IMM(-1));