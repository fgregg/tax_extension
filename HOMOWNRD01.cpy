00001 *  --  H O M E O W N E R   M A S T E R  --
00002 *                                          *BYTES* *DESCRIPTION*
00003 *                                            1-130  HOMEOWNER-REC.
00004      05  HO-KEY.
00005 *                                            1-10   RECORD KEY
00006          10  HO-VOL    PIC 9(3)     COMP-3.
00007 *                                             1-2     VOLUME
00008          10  HO-PROP   PIC 9(15)    COMP-3.
00009 *                                             3-10    PROPERTY-NO.
00010      05  HO-TXCD       PIC 9(5)     COMP-3.
00011 *                                           11-13   TAXCODE
00012      05  HO-STAT1      PIC 9.
00013          88  HO-STAT1-NOT-RENEWED   VALUE 0.
00014          88  HO-STAT1-RENEWED       VALUE 1.
00015          88  HO-STAT1-NO            VALUE 2.
00016          88  HO-STAT1-INCOMPLETE    VALUE 3.
00017          88  HO-STAT1-UNDELVRBLE    VALUE 4.
00018 *                                           14      STATUS-1
00019      05  HO-STAT2      PIC 9.
00020          88  HO-STAT2-NOT-RENEWED   VALUE 0.
00021          88  HO-STAT2-RENEWED       VALUE 1.
00022 *                                           15      STATUS-2
00023      05  FILLER        PIC X.
00024 *                                           16      FILLER
00025      05  HO-CLASS      PIC 9(3)     COMP-3.
00026 *                                           17-18   CLASS
00027 *                                                    MAJOR-MINOR
00028      05  HO-YRAPPL     PIC 99.
00029 *                                           19-20   YEAR APPLIED
00030      05  HO-NPHE-STATUS  PIC XX.
00031 *                                           21-22   BASE YEAR
00032 *                                                   ESTABLISHER
00033 *                                                      STATUS
00034 *                                                      SA = SALE
00035 *                                                      CO = COE
00036 *                                                      TR = TRI
00037 *                                                      DI = DIV
00038      05  HO-PRORATE    PIC 9V9(6)   COMP-3.
00039 *                                           23-26   PRO-RATION
00040 *                                                    FACTOR
00041      05  HO-COOPQTY    PIC 9(5)     COMP-3.
00042 *                                           27-29   COOP-QUANTITY
00043      05  HO-OCCFAC     PIC 9999V9   COMP-3.
00044 *                                           30-32   OCCUPANCY FACT
00045      05  FILLER        PIC X.
00046 *                                           33-33   FILLER
00047      05  HO-EQFCTR     PIC 9V9(4)   COMP-3.
00048 *                                           34-36   EQUALIZED
00049 *                                                    FACTOR
00050      05  HO-H-V        PIC 9.
00051          88  HO-NOT-EXEMPT          VALUE 0.
00052          88  HO-HOMESTEAD-EXEMPT    VALUE 3.
00053          88  HO-VETERANS-EXEMPT     VALUE 4.
00054          88  HO-COOP-HOMESTD-EXEMPT VALUE 5.
00055 *                                           37      HOMESTEAD OR
00056 *                                                    VETERANS
00057 *                                                    EXEMPTION
00058      05  HO-ASSDVAL    PIC 9(9)     COMP-3.
00059 *                                           38-42   ASSESSED
00060 *                                                    VALUATION
00061      05  HO-EQVAL      PIC 9(9)     COMP-3.
00062 *                                           43-47   EQUALIZED
00063 *                                                    VALUATION
00064      05  HO-NPHE       PIC 9(9)     COMP-3.
00065 *                                           48-52      NPHE
00066 *                                                     AMOUNT
00067      05  HO-NPHE-BSYR  PIC 9(4).
00068 *                                           53-56      NPHE
00069 *                                                     BASE YEAR
00070      05  FILLER        PIC X(01).
00071 *                                           57-57   FILLER
00072      05  HO-NAME       PIC X(22).
00073 *                                           58-79   NAME
00074      05  HO-ADDR       PIC X(22).
00075 *                                           80-101  ADDRESS
00076      05  HO-CITY       PIC X(12).
00077 *                                          102-113  CITY
00078      05  HO-STATE      PIC XX.
00079 *                                          114-115  STATE
00080      05  HO-ZIP        PIC 9(9)     COMP-3.
00081 *                                          116-120  ZIPCODE
00082      05  HO-C-OF-E     PIC 9(5)     COMP-3.
00083 *                                          121-123  C OF E NO.
00084      05  HO-STAT3      PIC 9.
00085 *                                          124      STATUS-3
00086      05  HO-TEMP-ASSD  PIC 9(9)     COMP-3.
00087 *                                          125-129  TEMP ASSD
00088 *                                                     VALUE
00089      05  HO-TXTYP      PIC 9.
00090 *                                          130      TAX TYPE
00091 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
