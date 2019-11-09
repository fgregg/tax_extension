00001 *    -- FROZEN VALUE SELECTION FILE --     *BYTES* *DESCRIPTION  *
00002 *
00003      05  FVS-KEY.
00004 *                                            1-16   KEY
00005          10  FVS-DIVNO         PIC 9(15)    COMP-3.
00006 *                                            1-8    DIVISION NO.
00007          10  FVS-PROPNO        PIC 9(15)    COMP-3.
00008 *                                            9-16   PROPERTY NO.
00009      05  FVS-TXCD              PIC 9(5)     COMP-3.
00010 *                                           17-19   TAX CODE
00011      05  FVS-CLASS             PIC 9(3)     COMP-3.
00012 *                                           20-21   CLASS
00013      05  FVS-CURR-288-AV-VAL   PIC S9(13)  COMP-3.
00014 *                                           22-28   CURRENT 288
00015 *                                                    A/V VALUE
00016      05  FVS-FRST-AV-VAL       PIC S9(13)   COMP-3.
00017 *                                           29-35   FIRST TIME
00018 *                                                    A/V VALUE
00019      05  FVS-EXPR-288-AV-VAL   PIC S9(13)   COMP-3.
00020 *                                           36-42   EXPIRED 288
00021 *                                                    A/V VALUE
00022      05  FVS-TXCD-CNT          PIC S9(7)    COMP-3.
00023 *                                           43-46   TAX CODE COUNT
00024      05  FVS-CURR-288-PCT      PIC S9(9)    COMP-3.
00025 *                                           47-51   CURRENT 288
00026 *                                                    PERCENT
00027      05  FVS-FRST-PCT          PIC S9(9)    COMP-3.
00028 *                                           52-56   FIRST TIME
00029 *                                                    PERCENT
00030      05  FVS-EXPR-288-PCT      PIC S9(9)    COMP-3.
00031 *                                           57-61   EXPIRED 288
00032 *                                                    PERCENT
00033      05  FVS-CURR-288-VAL      PIC S9(13)   COMP-3.
00034 *                                           62-68   CURRENT 288
00035 *                                                    VALUE
00036      05  FVS-FRST-VAL          PIC S9(13)   COMP-3.
00037 *                                           69-75   FIRST TIME
00038 *                                                    VALUE
00039      05  FVS-EXPR-288-VAL      PIC S9(13)   COMP-3.
00040 *                                           76-82   EXPIRED 288
00041 *                                                    VALUE
00042      05  FILLER                PIC X(18).
00043 *                                           83-100  FILLER
