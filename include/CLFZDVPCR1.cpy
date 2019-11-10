00001 * -- FROZEN VALUE DIVISION PERCENTAGE FILE *BYTES* DESCIPTION*
00002 *
00003      05  FDP-KEY.
00004 *                                            1-11   KEY
00005          10  FDP-DIVNO         PIC 9(15)    COMP-3.
00006 *                                            1-8    DIVISION NO.
00007          10  FDP-TXCD          PIC 9(5)     COMP-3.
00008 *                                            9-11   TAX CODE
00009      05  FDP-CURR-288-PCNT     PIC S99V9(7) COMP-3.
00010 *                                           12-16   CURRENT 288
00011 *                                                    PCNT
00012      05  FDP-FRST-PCNT         PIC S99V9(7) COMP-3.
00013 *                                           17-21   FIRST TIME
00014 *                                                    PCNT
00015      05  FDP-TXCD-COUNT        PIC S9(7)    COMP-3.
00016 *                                           22-25   TAX CODE
00017 *                                                    COUNT
00018      05  FDP-CURR-288-VAL      PIC S9(13)   COMP-3.
00019 *                                           26-32   CURRENT 288
00020 *                                                    PCNT
00021      05  FDP-EXPR-288-VAL      PIC S9(13)   COMP-3.
00022 *                                           33-39   CURRENT 288
00023 *                                                    PCNT
00024      05  FDP-FRST-VAL          PIC S9(13)   COMP-3.
00025 *                                           40-46   FIRST TIME
00026 *                                                    PCNT
00027      05  FDP-EXP-INC           PIC S9(13)   COMP-3.
00028 *                                           47-53   EXPIRED
00029 *                                                   INCENTIVE
00030      05  FILLER                PIC X(7).
00031 *                                           54-60   FILLER
