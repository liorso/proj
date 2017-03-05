/* scheme/write_sob_fraction.asm
 * Take a pointer to a Scheme pair object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 WRITE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);

  MOV(R0, FPARG(0));

  MOV (R1, INDD(R0, 1));
  CMP(R1, IMM(0));
  JUMP_EQ(PRINT_ZERO);

  MOV (R1, INDD(R0, 2))
  CMP(R1, IMM(1));
  JUMP_EQ(PRINT_MONE);

  PUSH(INDD(R0, 2));
  PUSH(INDD(R0, 1));
  CALL(WRITE_INTEGER);
  DROP(1);
  PUSH(IMM('/'));
  CALL(PUTCHAR);
  DROP(1);
  CALL(WRITE_INTEGER);
  DROP(1);
  POP(R1);
  POP(FP);
  RETURN;

PRINT_ZERO:
  PUSH(IMM(0));
  CALL(WRITE_INTEGER);
  DROP(1);
  POP(R1);
  POP(FP);
  RETURN;

PRINT_MONE:
  MOV(R0, FPARG(0));
  PUSH(INDD(R0, 1));
  CALL(WRITE_INTEGER);
  DROP(1);
  POP(R1);
  POP(FP);
  RETURN;
