/* scheme/make_sob_fraction.asm
 * Take pointers to two sexprs, and place the corresponding 
 * Scheme pair in R0
 * 
 * Programmer: Mayer Goldberg, 2010
 */


 MAKE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  
  MOV(R1, FPARG(0))
  PUSH(R1);
  CALL(ABS);
  DROP(1);
  PUSH(R0);

  MOV(R2, FPARG(1));
  PUSH(R2);
  CALL(ABS);
  DROP(1);
  PUSH(R0)

  CALL(GCD);
  DROP(2);
  PUSH(R0);

  MOV(R1, FPARG(0));

  PUSH(R1);
  CALL(ABS);
  DROP(1);

  MOV (R1, R0);
  MOV(R3, FPARG(0));
  MOV(R2, FPARG(1));
  DIV(R3, R1);
  MUL(R2, R3);
  POP(R0);
  DIV(R1, R0);
  DIV(R2, R0);
  PUSH(R1);
  PUSH(R2);

 

  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_FRACTION);
  POP(R2);
  MOV(INDD(R0, 1), R2);
  POP(R1);
  MOV(INDD(R0, 2), R1);

  POP(FP);
  RETURN;


GCD:
  PUSH(FP)
  MOV(FP, SP)

  /*CMP(FPARG(1), IMM(2));
  JUMP_NE(L_ERROR_INCORRECT_NUMBER_OF_ARGS);
  */
  MOV(R1, FPARG(0));
  MOV(R2, FPARG(1));
  CMP(R2, IMM(0));
  JUMP_EQ(L_SECOND_ARG_CANNOT_BE_ZERO);

GCD_LOOP:
  MOV(R3, R1);
  REM(R3, R2);
  CMP(R3, IMM(0));
  JUMP_EQ(GCD_EXIT);
  MOV(R1, R2);
  MOV(R2, R3);
  JUMP(GCD_LOOP);

GCD_EXIT:
  MOV(R0, R2);

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


 