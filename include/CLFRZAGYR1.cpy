00001 * --------- FROZEN AGENCY FILE ----------  *BYTES* DESCIPTION*
00002 *
00003      05  FA-KEY.
00004 *                                            1-6    KEY
00005          10  FA-TXCD         PIC 9(5)       COMP-3.
00006 *                                            1-3    TAX CODE
00007          10  FA-AGCY         PIC 9(9)       COMP-3.
00008 *                                            4-8    AGENCY
00009      05  FA-TXRTE            PIC 9(3)V9(3)  COMP-3.
00010 *                                            9-12   TAX RATE
00011      05  FA-CURR-288         PIC S9(13)     COMP-3.
00012 *                                           13-19   CURRENT 288
00013      05  FA-EXPR-288         PIC S9(13)     COMP-3.
00014 *                                           20-26   EXPIRED 288
00015      05  FA-FRST-TIME        PIC S9(13)     COMP-3.
00016 *                                           27-33   FIRST TIME
00017      05  FA-TOT-FROZ         PIC S9(13)     COMP-3.
00018 *                                           34-40   TOTAL FROZEN
00019      05  FA-FROZ-EQLZD       PIC S9(13)     COMP-3.
00020 *                                           41-47    FROZEN
00021 *                                                   EQUALIZED
00022      05  FA-FROZ-TXAMT       PIC S9(13)V99  COMP-3.
00023 *                                           48-55    FROZEN
00024 *                                                   TAX AMOUNT
00025      05  FA-ANNEXEDAV        PIC S9(13)     COMP-3.
00026 *                                           56-62    ANNEXED
00027 *                                                   ASSESSED VAL
00028      05  FA-ANNEXEDEV        PIC S9(13)     COMP-3.
00029 *                                           63-69    ANNEXED
00030 *                                                   EQUALIZED VAL
00031      05  FA-DISCONAV         PIC S9(13)     COMP-3.
00032 *                                           70-76    DISCONNECTED
00033 *                                                   ASSESSED VAL
00034      05  FA-DISCONEV         PIC S9(13)     COMP-3.
00035 *                                           77-83    DISCONNECTED
00036 *                                                   EQUALIZED VAL
00037      05  FA-TIFAV            PIC S9(13)     COMP-3.
00038 *                                           84-90    TIF
00039 *                                                   PRIOR YEAR
00040 *                                                   FROZEN
00041 *                                                   EQUALIZED VAL
00042      05  FA-TIFEV            PIC S9(13)     COMP-3.
00043 *                                           91-97    TIF
00044 *                                                   DIFFERENCE
00045 *                                                   EQUALIZED VAL
00046      05  FA-CURREV           PIC S9(13)     COMP-3.
00047 *                                           98-104   TIF
00048 *                                                   CURRENT
00049 *                                                   EQUALIZED VAL
00050      05  FA-EXPINC           PIC S9(11)     COMP-3.
00051 *                                           105-110  EXPIRED
00052 *                                                   INCENTIVES
00053      05  FA-EXPINCEV         PIC S9(11)     COMP-3.
00054 *                                           111-116  EXPIRED
00055 *                                                   INCENTIVES
00056 *                                                   EQUALIZED VAL
00057      05  FA-EXPINCTX         PIC S9(9)V99   COMP-3.
00058 *                                           117-122  EXPIRED
00059 *                                                   INCENTIVES
00060 *                                                   TAX AMOUNT
