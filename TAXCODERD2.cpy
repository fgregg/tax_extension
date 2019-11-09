00001 *                                           *BYTES* *DESCRIPTION*
00002      05 TAXCODE-REC.
00003 *                                            1-207
00004         10 TR-TAX-CODE         PIC 9(5)      COMP-3.
00005         10 TR-RATE             PIC 9(7)      COMP-3.
00006         10 TR-RATE-R REDEFINES TR-RATE
00007                                PIC 9(4)V9(3) COMP-3.
00008         10 TR-AGENCY OCCURS 40 TIMES
00009                                PIC 9(9)      COMP-3.
