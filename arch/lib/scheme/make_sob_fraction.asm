/* scheme/make_sob_fraction.asm
 * Take pointers to two sexprs, and place the corresponding 
 * Scheme pair in R0
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MAKE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_FRACTION);
  MOV(INDD(R0, 2), FPARG(0));
  MOV(INDD(R0, 1), FPARG(1));
  POP(FP);
  RETURN;


GCD:
  PUSH(FP)
  MOV(FP, SP)

  CMP(FPARG(1), IMM(2));
  JUMP_NE(L_ERROR_INCORRECT_NUMBER_OF_ARGS);
  MOV(R1, FPARG(2))
  CMP(IND(R1), IMM(T_INTEGER));
  JUMP_NE(L_ERROR_NOT_AN_INTEGER);
  MOV(R2, FPARG(3));
  CMP(IND(R2), IMM(T_INTEGER));
  JUMP_NE(L_ERROR_NOT_AN_INTEGER);
  CMP(INDD(R2, 1), IMM(0));
  JUMP_EQ(L_SECOND_ARG_CANNOT_BE_ZERO);
  MOV(R1, INDD(R1, 1));
  MOV(R2, INDD(R2, 1));

GCD_LOOP:
  MOV(R3, R1);
  REM(R3, R2);
  CMP(R3, IMM(0));
  JUMP_EQ(GCD_EXIT);
  MOV(R1, R2);
  MOV(R2, R3);
  JUMP(GCD_LOOP);

GCD_EXIT:
  PUSH(R2);
  CALL(MAKE_SOB_INTEGER);
  DROP(1);

  POP(FP);
  RETURN;


L_ERROR_INCORRECT_NUMBER_OF_ARGS:
  fprintf(stderr, "Procedure called with an incorrect number of arguments\n");
  exit(-1);

L_ERROR_NOT_AN_INTEGER:
  fprintf(stderr, "Procedure called with an non-integer argument\n");
  exit(-1);


L_SECOND_ARG_CANNOT_BE_ZERO:
  fprintf(stderr, "The second argument cannot be zero\n");
  exit(-1);