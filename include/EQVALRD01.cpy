00001      05 EQ-VOL            PIC 999      COMP-3.
00002 *                                     1-2         VOLUME
00003      05 EQ-PROP           PIC 9(15)    COMP-3.
00004 *                                     3-10        PROPERTY NUMBER
00005      05 EQ-TXCD           PIC 9(5)     COMP-3.
00006 *                                    11-13        TAX CODE
00007      05 EQ-ASSD-VAL       PIC S9(11)   COMP-3.
00008 *                                    14-19        ASSESSED VALUE
00009      05 EQ-EQ-FC          PIC S9(5)    COMP-3.
00010      05 EQ-EQ-FCR REDEFINES EQ-EQ-FC
00011                           PIC S9V9(4)  COMP-3.
00012 *                                    20-22        EQUALIZATION FAC
00013      05 EQ-EQ-VAL         PIC S9(11)   COMP-3.
00014 *                                    23-28        EQUALIZED VALUE
00015      05 EQ-STAT           PIC 9.
00016 *                                    29-29        STATUS
00017      05 EQ-MA-MI.
00018         10 EQ-MA-CLS      PIC 9.
00019 *                                    30-30        MAJOR CLASS
00020         10 EQ-MI-CLS      PIC 99.
00021 *                                    31-32        MINOR CLASS
00022      05 EQ-CLASS REDEFINES EQ-MA-MI
00023                           PIC 9(3).
00024 *                                    30-32          CLASS
00025      05 EQ-SC-HMESTD-AMT  PIC S9(11)   COMP-3.
00026 *                                    33-38         HOMESTEAD AMOUN
00027      05 EQ-VET-EX-AMT     PIC S9(11)   COMP-3.
00028 *                                    39-44         VETERAN  AMOUNT
00029      05 EQ-HOME-AMT       PIC S9(11)   COMP-3.
00030 *                                    45-50         VETERAN  AMOUNT
00031      05 EQ-EZ-ABATAMT     PIC S9(11)   COMP-3.
00032 *                                    51-56         EZ ABATEMENT AM
00033      05 EQ-TXTYP          PIC 9.
00034         88  EQ-CUR-TXTYP               VALUE 0.
00035         88  EQ-AIR-POLL-TXTYP          VALUE 3.
00036         88  EQ-CIRCULATOR              VALUE 5.
00037 *                                    57-57         TAX TYPE
00038      05 EQ-SRFRZEXVAL     PIC 9(11)    COMP-3.
00039 *                                    58-63        SENIOR FREEZE AM
00040      05 EQ-IA-IND         PIC X.
00041 *                                    64-64        INDICATOR
00042      05 EQ-EXOWNEXVAL     PIC 9(11)    COMP-3.
00043 *                                    65-70        EX HOMEONER VAL
00044      05 EQ-DIS-IND        PIC X.
00045         88  RETURNING-VET       VALUE '1'.
00046         88  DISABLED-PERSON     VALUE '2'.
00047         88  DISABLED-VET1       VALUE '3'.
00048         88  DISABLED-VET2       VALUE '4'.
00049         88  RET-DIS-VET1        VALUE '5'.
00050         88  RET-DIS-VET2        VALUE '6'.
00051         88  RET-DIS-PER         VALUE '7'.
00052         88  DISABLED-VET3       VALUE '8'.
00053 *           1 = RETURNING VETERAN
00054 *           2 = DISABLED PERSON
00055 *           3 = DISABLED VETERAN 30 - 49%
00056 *           4 = DISABLED VETERAN 50 - 69%
00057 *           5 = RETURING VETERAN / DISABLED VETERAN  30-49%
00058 *           6 = RETURNING VETERAN / DISABLED VETERAN 50-69%
00059 *           7 = RETURNING VETERAN / DISABLED PERSON
00060 *           8 = DISABLED  VETERAN >= 70%
00061 *
00062 *                                    71-71        DISABLED EXEMPTI
00063 *                                                     INDICATOR
00064      05 EQ-RET-VET        PIC 9(11)    COMP-3.
00065 *                                    72-77        RETURNING VETERA
00066      05 EQ-DIS-PER        PIC 9(11)    COMP-3.
00067 *                                    78-83        DISABLED PERSON
00068      05 EQ-DIS-VET-1      PIC 9(11)    COMP-3.
00069 *                                    84-89        DISABLED VETERAN
00070 *                                                   30-49%
00071      05 EQ-DIS-VET-2      PIC 9(11)    COMP-3.
00072 *                                    90-95        DISABLED VETERN
00073 *                                                  50-69%
00074      05 EQ-HOM-IND        PIC X.
00075 *       1 = < 75,000
00076 *       2 = > 75,000
00077 *       3 = SUNSET
00078 *                                    96-96        LONGTIME HOMEOWN
00079 *                                                     INDICATOR
00080      05 EQ-DIS-VET-3      PIC 9(11)    COMP-3.
00081 *                                    97-102       DISABLED VETERN
00082 *                                                  > =     70%
00083      05 FILLER            PIC X(13).
00084 *                                   103-115       FILLER
