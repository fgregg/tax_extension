00001 *  --         S E N I O R   F R E E Z E   F I L E            --
00002 *                                          *BYTES* *DESCRIPTION*
00003 *                                            1-135  SENIOR
00004 *                                                     FREEZE
00005 *                                                     RECORD
00006 *                                                     BASE
00007      05  SR-KEY.
00008 *                                            1-10   RECORD KEY
00009          10  SR-VOL    PIC 9(3)     COMP-3.
00010 *                                             1-2     VOLUME
00011          10  SR-PROP   PIC 9(15)    COMP-3.
00012 *                                             3-10    PROPERTY-NO.
00013      05  SR-TXCD       PIC 9(5)     COMP-3.
00014 *                                           11-13   TAXCODE
00015      05  SR-STAT1      PIC 9.
00016          88  SR-STAT1-NOT-RENEWED   VALUE 0.
00017          88  SR-STAT1-RENEWED       VALUE 1.
00018          88  SR-STAT1-NO            VALUE 2.
00019          88  SR-STAT1-INCOMPLETE    VALUE 3.
00020          88  SR-STAT1-UNDELVRBLE    VALUE 4.
00021 *                                           14      STATUS-1
00022      05  SR-STAT2      PIC 9.
00023          88  SR-STAT2-NOT-RENEWED   VALUE 0.
00024          88  SR-STAT2-RENEWED       VALUE 1.
00025 *                                           15      STATUS-2
00026      05  SR-CLERKS-CLS PIC 9.
00027 *                                           16      CLERK'S
00028 *                                                    CLASS
00029      05  SR-CLASS      PIC 9(3)     COMP-3.
00030 *                                           17-18   CLASS
00031 *                                                    MAJOR-MINOR
00032      05  SR-YRAPPL     PIC 99.
00033 *                                           19-20   YEAR APPLIED
00034      05  SR-IND        PIC 9.
00035 *                                           21      ELIGIBILITY
00036 *                                                    INDICATOR
00037      05  FILLER        PIC X.
00038 *                                           22      FILLER
00039      05  SR-PRORATE    PIC 9V9(6)   COMP-3.
00040 *                                           23-26   PRO-RATION
00041 *                                                    FACTOR
00042      05  SR-COOPQTY    PIC 9(5)     COMP-3.
00043 *                                           27-29   COOP-QUANTITY
00044      05  SR-OCCFAC     PIC 9999V9   COMP-3.
00045 *                                           30-32   OCCUPANCY FACT
00046      05  SR-SPLIT-CD   PIC 9.
00047 *                                           33-33   SPLIT CODE
00048      05  SR-EQFCTR     PIC 9V9(4)   COMP-3.
00049 *                                           34-36   EQUALIZED
00050 *                                                    FACTOR
00051      05  SR-H-V        PIC 9.
00052          88  SR-NOT-EXEMPT          VALUE 0.
00053          88  SR-HOMESTEAD-EXEMPT    VALUE 3.
00054          88  SR-VETERANS-EXEMPT     VALUE 4.
00055          88  SR-COOP-HOMESTD-EXEMPT VALUE 5.
00056 *                                           37      HOMESTEAD OR
00057 *                                                    VETERANS
00058 *                                                    EXEMPTION
00059      05  SR-ASSDVAL    PIC 9(9)     COMP-3.
00060 *                                           38-42   ASSESSED
00061 *                                                    VALUATION
00062      05  SR-EQVAL      PIC 9(9)     COMP-3.
00063 *                                           43-47   EQUALIZED
00064 *                                                    VALUATION
00065      05  SR-KEYPCL     PIC 9(15)    COMP-3.
00066 *                                           48-55   KEYPARCEL NO.
00067      05  FILLER        PIC XX.
00068 *                                           56-57   FILLER
00069      05  SR-NAME       PIC X(22).
00070 *                                           58-79   NAME
00071      05  SR-ADDR       PIC X(22).
00072 *                                           80-101  ADDRESS
00073      05  SR-CITY       PIC X(12).
00074 *                                          102-113  CITY
00075      05  SR-STATE      PIC XX.
00076 *                                          114-115  STATE
00077      05  SR-ZIP        PIC 9(9)     COMP-3.
00078 *                                          116-120  ZIPCODE
00079      05  SR-C-OF-E     PIC 9(5)     COMP-3.
00080 *                                          121-123  C OF E NO.
00081      05  SR-STAT3      PIC 9.
00082 *                                          124      STATUS-3
00083      05  FILLER        PIC X(4).
00084 *                                          125-128  FILLER
00085      05  SR-RECCD      PIC 9.
00086 *                                          129      REC CODE
00087      05  SR-TXTYP      PIC 9.
00088 *                                          130      TAX TYPE
00089      05  SR-HMCNTR     PIC 9(5).
00090 *                                          131-135  HOMESTEAD
00091 *                                                     SEGMENT
00092 *                                                     COUNTER
00093      05  SR-HMSTSEG OCCURS 0 TO 410 TIMES
00094             DEPENDING ON SR-HMCNTR.
00095 *                                                   HOMESTEAD
00096 *                                                     SEGMENT
00097              10  SR-HSG-BRTHDTE   PIC 9(6).
00098 *                                            1-6    BIRTH DATE
00099              10  SR-HSG-NAME      PIC X(22).
00100 *                                            7-28   NAME
00101              10  SR-HSG-ADDR      PIC X(22).
00102 *                                           29-50   ADDRESS
00103              10  SR-HSG-CITY      PIC X(12).
00104 *                                           51-62   CITY
00105              10  SR-HSG-STATE     PIC XX.
00106 *                                           63-64   STATE
00107              10  SR-HSG-ZIP       PIC 9(9)  COMP-3.
00108 *                                           65-69   ZIPCODE
00109              10  SR-HSG-YRAPPL    PIC 9(4).
00110 *                                           70-73   YEAR APPLIED
00111              10  SR-HSG-BATCH     PIC 9(5)  COMP-3.
00112 *                                           74-76   BATCH
00113 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
