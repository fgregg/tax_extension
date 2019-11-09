00001 *         *  ASSESSOR'S SENIOR FREEZE EXEMPTION DATA FILE  *
00002 *
00003 *  CHANGED 06/2000   EX-PCTSENSHRS
00004 *
00005      03  EX-TOWN-NO         PIC 9(3).
00006 *                                          1-3    TOWN NO.
00007      03  EX-VOLUME          PIC 9(3).
00008 *                                          4-6    VOLUME
00009      03  EX-PROP-NO         PIC 9(14).
00010 *                                         7-20    PROPERTY NO.
00011      03  EX-YEAR            PIC 99.
00012 *                                        21-22    YEAR
00013      03  EX-TAX-YEAR        PIC 99.
00014 *                                        23-24    YEAR
00015      03  EX-TAX-TYPE        PIC 9.
00016 *                                        25-25    TAX TYPE
00017      03  EX-SENIOR-FREEZE   PIC X(390).
00018 *                                        26-415   SENIOR FREEZE
00019 *                                                     DATA
00020      03  EX-SENIOR-FREEZE-R REDEFINES EX-SENIOR-FREEZE.
00021          05  EX-FILLER1     PIC X(359).
00022 *                                        26-384   FILLER
00023          05  EX-SFPCT       PIC 9(2).
00024 *                                       385-386   SENIOR FREEZE
00025 *                                                     PERCENT
00026          05  EX-FILLER2     PIC X(29).
00027 *                                       387-415   FILLER
00028      03  EX-BIRTHDATE       PIC X(8).
00029 *                                       416-423   BIRTH DATE
00030 *                                                 (MMDDCCYY)
00031      03  EX-APPOLDNAM       PIC X(22).
00032 *                                       424-445   APPLICANT
00033 *                                                  OLD NAME
00034      03  EX-SFSTAT          PIC X.
00035 *                                       446-446   SENIOR FREEZE
00036 *                                                  STATUS
00037      03  EX-FSTAPPDTE       PIC 9(8).
00038 *                                       447-454   FIRST
00039 *                                                 APPLICATION
00040 *                                                 DATE (CCYYMMDD)
00041      03  EX-LSTAPPDTE       PIC 9(8).
00042 *                                       455-462   LAST APPLICATION
00043 *                                                 DATE (CCYYMMDD)
00044      03  EX-QUALDTE         PIC 9(8).
00045 *                                       463-470   QUALIFY DATE
00046 *                                                  (CCYYMMDD)
00047      03  EX-RTNDDTE         PIC 9(8).
00048 *                                       471-478   RETURN DATE
00049 *                                                  (CCYYMMDD)
00050      03  EX-MANRTNCODES.
00051 *                                       479-498   MANUAL RETURN
00052 *                                                    CODES
00053          05  EX-MANRTNCD    PIC 99 OCCURS 10 TIMES.
00054 *
00055      03  EX-MISRTNCODES.
00056 *                                       499-518   M.I.S.  RETURN
00057 *                                                     CODES
00058          05  EX-MISRTNCD    PIC 99 OCCURS 10 TIMES.
00059 *
00060      03  EX-COOPSENSHRS     PIC 9(6).
00061 *                                       519-524   COOP SENIOR
00062 *                                                   SHARES
00063 *    03  EX-PCTSENSHRS      PIC 9(6).
00064      03  EX-PCTSENSHRS      PIC V9(6).
00065 *                                       525-530   PERCENT SENIOR
00066 *                                                   SHARES
00067      03  EX-HSSTAT          PIC X.
00068 *                                       531-531   HOMESTEAD STATUS
00069      03  EX-SHARES          PIC 9(6).
00070 *                                       532-537   HOMESTEAD SHARES
00071      03  EX-PCTSHARES       PIC 9(3)V9(3).
00072 *                                       538-543   HOMESTEAD
00073 *                                                 PERCENTAGE
00074 *                                                 OF SHARES
00075      03  EX-HOSTAT          PIC X.
00076 *                                       544-544   HOMEOWNER STATUS
00077      03  EX-HOBASYR         PIC 9(4).
00078 *                                       545-548   HOMEOWNER
00079 *                                                 BASE YEAR
00080      03  EX-HOBYEQFAC       PIC 9V9(4).
00081 *                                       549-553   HOMEOWNER BASE
00082 *                                                 YEAR EQUALIZED
00083 *                                                 FACTOR
00084      03  EX-HOBYAV          PIC 9(9).
00085 *                                       554-562   HOMEOWNER BASE
00086 *                                                 YEAR ASSESSED
00087 *                                                 VALUATION
00088      03  EX-HOBYEV          PIC 9(9).
00089 *                                       563-571   HOMEOWNER BASE
00090 *                                                 YEAR EQUALIZED
00091 *                                                 VALUATION
00092      03  EX-ELGIND          PIC 9.
00093 *                                       572-572   ELIGIBLE
00094 *                                                 INDICATOR
00095      03  EX-MAINTIND        PIC 9.
00096 *                                       573-573   MAINTENANCE
00097 *                                                 INDICATOR
00098      03  EX-MTTERMID        PIC X(4).
00099 *                                       574-577   MAINTENANCE
00100 *                                                 TERMINAL I.D.
00101      03  EX-MUTPID          PIC X(8).
00102 *                                       578-585   MAINTENANCE
00103 *                                                 UPDATE I.D.
00104      03  EX-BASE-YR         PIC 9(4).
00105 *                                       586-589   SENIOR FEEZE
00106 *                                                   BASE YEAR
00107      03  EX-BSYR-IND        PIC X.
00108 *                                       590-590   SENIOR FEEZE
00109 *                                                 BASE YEAR
00110 *                                                 INDICATOR
00111      03  EX-BSYR-EQVAL      PIC 9(9).
00112 *                                       591-599   SENIOR FEEZE
00113 *                                                 BASE YEAR
00114 *                                                 EQUALIZED
00115 *                                                 VALUATION
00116      03  EX-CALC-TYP        PIC X.
00117 *                                       600-600   CALCULATION TYPE
