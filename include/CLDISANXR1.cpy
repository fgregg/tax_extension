00001 *                                           *BYTES* *DESCRIPTION*
00002      05 DA-DIVNO          PIC 9(14)     COMP-3.
00003 *                                            1-8      DIVISION
00004 *                                                     NUMBER
00005      05 DA-TOTPREV        PIC 9(13)     COMP-3.
00006 *                                            9-15     TOTAL PRIOR
00007 *                                                     EQUALIZED
00008 *                                                     VALUATION
00009      05 DA-TOTCUEV        PIC 9(13)     COMP-3.
00010 *                                           16-22     TOTAL CURREN
00011 *                                                     EQUALIZED
00012 *                                                     VALUATION
00013      05 DA-PCTCHG         PIC S999V9(8) COMP-3.
00014 *                                           23-28     PERCENTAGE
00015 *                                                     OF CHANGE
00016      05 FILLER            PIC X(39).
00017 *                                           29-67     FILLER
00018      05 DA-SEGCTR         PIC 9(3).
00019 *                                           68-70     SEGMENT
00020 *                                                     COUNTER
00021      05 DA-SEG OCCURS 0 TO 50 TIMES DEPENDING ON DA-SEGCTR.
00022 *                                           71-80
00023 *                                                 THRU 561-570
00024         10 DA-SEG-TYP     PIC X.
00025 *                                           71-71     TYPE
00026 *                                                     (A OR D)
00027         10 DA-SEG-AGCY    PIC 9(9).
00028 *                                           72-80     AGENCY NO.
00029         10 FILLER         PIC X(5).
00030 *                                           81-85     FILLER
