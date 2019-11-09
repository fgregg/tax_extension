00001 *----------------------------------------------------------------*
00002 *                      *  H O M E O W N E R  *
00003      05  PO-HOMEOWNR.
00004 *                                              1-170 HOMEOWNER
00005          10  PO-SEGCODE     PIC X.
00006              88  PO-HOMEOWNER       VALUE 'O'.
00007 *                                              1     KEY -
00008 *                                                     SEGMENT CODE
00009          10  PO-PRT-IND     PIC 9.
00010              88  PO-NOT-PRINTED     VALUE 0.
00011              88  PO-PRINTED         VALUE 1.
00012 *                                              2     PRINT
00013 *                                                     INDICATOR
00014          10  PO-RESPONSE    PIC 9.
00015              88  PO-NO-RESPONSE     VALUE 0.
00016              88  PO-RESPOND-YES     VALUE 1.
00017              88  PO-RESPOND-NO      VALUE 2.
00018              88  PO-INCOMPLETE      VALUE 3.
00019              88  PO-UNDELIVERABLE   VALUE 4.
00020 *                                              3     RESPONSE
00021          10  PO-YR-APPLD    PIC 99.
00022 *                                              4-5   YEAR APPLIED
00023          10  PO-MAINT-IND   PIC 9.
00024              88  PO-NOT-CHANGED     VALUE 0.
00025              88  PO-CHANGED         VALUE 1.
00026 *                                              6     MAINTENANCE
00027 *                                                     INDICATOR
00028          10  PO-PROFCTR     PIC 9V9(6)   COMP-3.
00029 *                                              7-10  PRORATION
00030 *                                                     FACTOR
00031          10  PO-COOPQTY     PIC 9(5)     COMP-3.
00032 *                                             11-13  COOP QUANTITY
00033          10  PO-COOPSTAT    PIC 9.
00034              88  PO-NON-COOP        VALUE 1.
00035              88  PO-COOP            VALUE 2.
00036 *                                             14     COOP STATUS
00037 *                                                                *
00038 *                                     *  15-27 BASE YEAR FIELDS  *
00039          10  PO-EQFCTR      PIC 9V9(4)   COMP-3.
00040 *                                             15-17  EQUALIZED
00041 *                                                     FACTOR
00042          10  PO-ASSDVAL     PIC 9(9)     COMP-3.
00043 *                                             18-22  ASSESSED
00044 *                                                     VALUATION
00045          10  PO-EQVAL       PIC 9(9)     COMP-3.
00046 *                                             23-27  EQUALIZED
00047 *                                                     VALUATION
00048          10  PO-BATCH       PIC 9(5)     COMP-3.
00049 *                                             28-30  BATCH NUNBER
00050          10  PO-OCCFAC      PIC 9999V9   COMP-3.
00051 *                                             31-33  OCCUPANCY
00052 *                                                     FACTOR
00053          10  PO-NPHE-AMT    PIC 9(9)     COMP-3.
00054 *                                             34-37     NPHE
00055 *                                                      AMOUNT
00056          10  PO-NPHE-BSYR   PIC 9(4).
00057 *                                             38-41     NPHE
00058 *                                                     BASE YEAR
00059          10  PO-NPHE-STATUS PIC X(2).
00060 *                                             42-43  NPHE STATUS
00061          10  FILLER         PIC X(126).
00062 *                                             44-170 FILLER
00063 *----------------------------------------------------------------*
