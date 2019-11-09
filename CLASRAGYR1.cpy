00001 *                                           *BYTES* *DESCRIPTION*
00002      05 AGENCY-MASTER-REC.
00003 *                                            1-275
00004         10 AM-VOL         PIC 9(3)      COMP-3.
00005 *                                            1-2      VOLUME
00006         10 AM-PROP        PIC 9(15)     COMP-3.
00007 *                                            3-10     PROPERTY
00008 *                                                     NUMBER
00009         10 AM-TXTYP       PIC 9.
00010 *                                            11       TAX TYPE
00011         10 AM-TXCD        PIC 9(5).
00012 *                                            12-16    TAX CODE
00013         10 AM-CLASS       PIC 9(3)      COMP-3.
00014 *                                            17-18    CLASS
00015         10 AM-STAT        PIC 9.
00016 *                                            19       STATUS
00017         10 AM-DIVNO       PIC 9(15)     COMP-3.
00018 *                                            20-27    DIVISION
00019 *                                                     NUMBER
00020         10 AM-ASSD-VAL    PIC 9(11)     COMP-3.
00021 *                                            28-33    ASSESSED
00022 *                                                     VALUATION
00023         10 AM-EQUL-VAL    PIC 9(11)     COMP-3.
00024 *                                            34-39    EQUALIZED
00025 *                                                     VALUATION
00026         10 FILLER         PIC X(3).
00027 *                                            40-42    FILLER
00028         10 AM-RATE        PIC 9(7)      COMP-3.
00029         10 AM-RATE-R REDEFINES AM-RATE
00030                           PIC 9(4)V9(3) COMP-3.
00031 *                                            43-46    TAX CODE
00032 *                                                     RATE
00033         10 AM-AGENCY OCCURS 40 TIMES
00034                           PIC 9(9)      COMP-3.
00035 *                                            47-246   AGENCIES
00036         10 FILLER         PIC X(29).
00037 *                                            247-275  TOTAL
