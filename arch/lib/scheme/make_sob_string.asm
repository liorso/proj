/* scheme/make_sob_string.asm
 * Takes CHAR1, ..., CHARn, n, on the stack. Places in R0 the address
 * of a newly-allocated pointer to a Scheme string.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MAKE_SOB_STRING:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  MOV(R0, FPARG(0));
  ADD(R0, IMM(2));
  PUSH(R0);
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), IMM(T_STRING));
  MOV(INDD(R0, 1), FPARG(0));
  MOV(R1, FP);
  MOV(R2, FPARG(0));
  ADD(R2, IMM(3));
  SUB(R1, R2);
  MOV(R2, R0);
  ADD(R2, IMM(2));
  MOV(R3, FPARG(0));
 L_MSS_LOOP:
  CMP(R3, IMM(0));
  JUMP_EQ(L_MSS_EXIT);
  MOV(IND(R2), STACK(R1)); 
  INCR(R1);
  INCR(R2);
  DECR(R3);
  JUMP(L_MSS_LOOP);
 L_MSS_EXIT:
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;

IS_STRINGS_EQUAL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  PUSH(R4);
  MOV(R1, IND(FPARG(0)));
  MOV(R2, IND(FPARG(1)));
  MOV(R3, INDD(FPARG(0), 1));
  MOV(R4, IMM(0));
  ADD(R3, IMM(2));
CMP_LOOP:
  CMP(R3, IMM(0));
  JUMP_EQ(STRINGS_EQUAL);
  CMP(R1, R2);
  JUMP_NE(STRINGS_NOT_EQUAL);
  INCR(R4);
  DECR(R3);
  MOV(R1, INDD(FPARG(0), R4));
  MOV(R2, INDD(FPARG(1), R4));
  JUMP(CMP_LOOP);
STRINGS_NOT_EQUAL:
  MOV(R0, IMM(0));
  JUMP(CMP_EXIT);
STRINGS_EQUAL:
  MOV(R0, IMM(1));
  JUMP(CMP_EXIT);
CMP_EXIT:
  POP(R4);
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;