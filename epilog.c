
JUMP(BEFORE_END);

NOT_CLOSURE:
SHOW("ERROR: NOT CLOSURE!!!", INDD(R0,0));
JUMP(BEFORE_END);

ERROR_NUM_OF_ARG:
SHOW("ERROR: NUM OF ARGS!!!", FPARG(1));
JUMP(BEFORE_END);

ERROR:
SHOW("GENERAL ERROR!!!!!!!", FPARG(1));
JUMP(BEFORE_END);

DELETE_ME:
SHOW("I am dani's work! it OK!!!", FPARG(0));
JUMP(BEFORE_END);

NOT_CHAR2:
INFO;
SHOW("NOT CHAR22222", FPARG(1));
JUMP(BEFORE_END);

BEFORE_END:
DROP(4);
CMP(R0,IMM(722689));
JUMP_EQ(END);
END:
STOP_MACHINE;

return 0;
}
