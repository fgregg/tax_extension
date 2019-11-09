00001 *  --  H O M E O W N E R   M A S T E R  (2ND RECORD DESCRIPTION)--
00002 *                                          *BYTES* *DESCRIPTION*
00003 *                                            1-130  HOMEOWNER-REC.
00004      05  HO-KEY2.
00005 *                                            1-10   RECORD KEY
00006          10  HO-VOL2   PIC 9(3)     COMP-3.
00007 *                                             1-2     VOLUME
00008          10  HO-PROP2  PIC 9(15)    COMP-3.
00009 *                                             3-10    PROPERTY-NO.
00010      05  HO-TXCD2      PIC 9(5)     COMP-3.
00011 *                                           11-13   TAXCODE
00012      05  HO-STAT1-2    PIC 9.
00013 *                                           14      STATUS-1
00014      05  HO-STAT2-2    PIC 9.
00015 *                                           15      STATUS-2
00016      05  FILLER        PIC X.
00017 *                                           16      FILLER
00018      05  HO-CLASS2     PIC 9(3)     COMP-3.
00019 *                                           17-18   CLASS
00020 *                                                    MAJOR-MINOR
00021      05  HO-YRAPPL2    PIC 99.
00022 *                                           19-20   YEAR APPLIED
00023      05  HO-NPHE-STATUS-2  PIC XX.
00024 *                                           21-22   BASE YEAR
00025 *                                                   ESTABLISHER
00026 *                                                     STATUS
00027 *                                                     SA = SALE
00028 *                                                     CO = COE
00029 *                                                     TR = TRI
00030 *                                                     DI = DIV
00031      05  HO-PRORATE2   PIC 9V9(6)   COMP-3.
00032 *                                           23-26   PRO-RATION
00033 *                                                    FACTOR
00034      05  HO-COOPQTY2   PIC 9(5)     COMP-3.
00035 *                                           27-29   COOP-QUANTITY
00036      05  HO-OCCFAC2    PIC 9999V9   COMP-3.
00037 *                                           30-32   OCCUPANCY FACT
00038      05  FILLER        PIC X.
00039 *                                           33-33   FILLER
00040      05  HO-EQFCTR2    PIC 9V9(4)   COMP-3.
00041 *                                           34-36   EQUALIZED
00042 *                                                    FACTOR
00043      05  HO-H-V2       PIC 9.
00044 *                                           37      HOMESTEAD OR
00045 *                                                    VETERANS
00046 *                                                    EXEMPTION
00047      05  HO-ASSDVAL2   PIC 9(9)     COMP-3.
00048 *                                           38-42   ASSESSED
00049 *                                                    VALUATION
00050      05  HO-EQVAL2     PIC 9(9)     COMP-3.
00051 *                                           43-47   EQUALIZED
00052 *                                                    VALUATION
00053      05  HO-NPHE2      PIC 9(9)     COMP-3.
00054 *                                           48-52      NPHE
00055 *                                                     AMOUNT
00056      05  HO-NPHE-BSYR2  PIC 9(4).
00057 *                                           53-56      NPHE
00058 *                                                   BASE YEAR
00059      05  FILLER        PIC X(01).
00060 *                                           57-57   FILLER
00061      05  HO-NAME2      PIC X(22).
00062 *                                           58-79   NAME
00063      05  HO-ADDR2      PIC X(22).
00064 *                                           80-101  ADDRESS
00065      05  HO-CITY2      PIC X(12).
00066 *                                          102-113  CITY
00067      05  HO-STATE2     PIC XX.
00068 *                                          114-115  STATE
00069      05  HO-ZIP2       PIC 9(9)     COMP-3.
00070 *                                          116-120  ZIPCODE
00071      05  HO-C-OF-E2    PIC 9(5)     COMP-3.
00072 *                                          121-123  C OF E NO.
00073      05  HO-STAT3-2    PIC 9.
00074 *                                          124      STATUS-3
00075      05  HO-TEMP-ASSD2 PIC 9(9)     COMP-3.
00076 *                                          125-129  TEMP ASSD
00077 *                                                     VALUE
00078      05  HO-TXTYP2     PIC 9.
00079 *                                          130-130  TAX TYPE
00080 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
