00001 *                                              POS.   DESCRIPTION
00002 *
00003 *                                              1-275  AGENCY
00004 *                                                     RECORD
00005      05  PA-VOL             PIC 999   COMP-3.
00006 *                                              1-  2  VOLUME
00007      05  PA-PROP            PIC 9(15) COMP-3.
00008 *                                              3- 10  PROPERTY NO.
00009      05  PA-TXTYP           PIC 9.
00010 *                                             11- 11  TAX TYPE
00011      05  PA-TXCD            PIC 9(5).
00012 *                                             12- 16  TAX CODE
00013      05  PA-CLASS           PIC 999   COMP-3.
00014 *                                             17- 18  CLASS
00015      05  PA-STAT            PIC 9.
00016 *                                             19- 19  STATUS
00017      05  PA-DIVNO           PIC 9(15) COMP-3.
00018 *                                             20- 27  DIVISION
00019 *                                                     NO.
00020      05  PA-ASSD-VAL        PIC 9(11) COMP-3.
00021 *                                             28- 33  ASSESSED
00022 *                                                     VALUATION
00023      05  PA-EQUL-VAL        PIC 9(11) COMP-3.
00024 *                                             34- 39  EQUALIZED
00025 *                                                     VALUATION
00026      05  FILLER             PIC X(3).
00027 *                                             40- 42
00028      05  PA-RATE            PIC 9(7)  COMP-3.
00029 *                                             43- 46  TAX CODE
00030 *                                                     RATE
00031      05  PA-RATE-R          REDEFINES PA-RATE
00032                             PIC 9(4)V999 COMP-3.
00033 *                                             43- 46  TAX CODE
00034 *                                                     RATE
00035      05  PA-AGENCY          OCCURS 40 TIMES
00036                             PIC 9(9)  COMP-3.
00037 *                                             47-246  AGENCY
00038      05  FILLER             PIC X(29).
00039 *                                            247-275  TOTAL
