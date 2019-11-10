00001 *    -- FROZEN VALUE TAX CODE FILE --      *BYTES* *DESCRIPTION
00002 *
00003      05  FVT-KEY.
00004 *                                            1-11   KEY
00005          10  FVT-DIVNO         PIC 9(15)    COMP-3.
00006 *                                            1-8    DIVISION NO.
00007          10  FVT-TXCD          PIC 9(5)     COMP-3.
00008 *                                            9-11   TAX CODE
00009      05  FVT-CURR-288-VAL      PIC S9(13)   COMP-3.
00010 *                                           12-18   CURRENT 288
00011 *                                                    VALUE
00012      05  FVT-FRST-VAL          PIC S9(13)   COMP-3.
00013 *                                           19-25   FIRST TIME
00014 *                                                    VALUE
00015      05  FILLER                PIC X(20).
00016 *                                           26-45   FILLER
