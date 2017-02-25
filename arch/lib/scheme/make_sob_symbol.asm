/* scheme/make_sob_symbol.asm
 * Take pointers to two sexprs, and place the corresponding 
 * Scheme symbol in R0
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MAKE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(2));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_SYMBOL);
  MOV(INDD(R0, 1), FPARG(0));
  POP(FP);
  RETURN;

