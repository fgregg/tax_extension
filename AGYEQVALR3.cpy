00001 *              -----------------           *BYTES* *DESCRIPTION*
00002      05  AGCY-EQV-KEY.
00003          07  AEV-AGCYNO    PIC 9(9)     PACKED-DECIMAL.
00004 *                                            1-5   AGENCY NUMBER
00005      05  AEV-CC-RE         PIC 9(13)    PACKED-DECIMAL.
00006 *                                            6-12  COOK COUNTY
00007 *                                            REAL ESTATE
00008      05  AEV-CC-AIRPOL     PIC 9(11)    PACKED-DECIMAL.
00009 *                                            13-18 COOK COUNTY
00010 *                                            AIR POLLUTION
00011      05  AEV-CC-USETAX     PIC 9(11)    PACKED-DECIMAL.
00012 *                                            19-24 COOK COUNTY
00013 *                                            USE TAX
00014      05  AEV-CC-RR         PIC 9(11)    PACKED-DECIMAL.
00015 *                                            25-30 COOK COUNTY
00016 *                                            RAILROAD
00017      05  AEV-DPG-EQV       PIC 9(11)    PACKED-DECIMAL.
00018 *                                            31-36 DUPAGE COUNTY
00019 *                                            EQUALIZED VALUATION
00020      05  AEV-LAK-EQV       PIC 9(11)    PACKED-DECIMAL.
00021 *                                            37-42 LAKE COUNTY
00022 *                                            EQUALIZED VALUATION
00023      05  AEV-KNK-EQV       PIC 9(11)    PACKED-DECIMAL.
00024 *                                            43-48 KANKAKEE COUNTY
00025 *                                            EQUALIZED VALUATION
00026      05  AEV-KND-EQV       PIC 9(11)    PACKED-DECIMAL.
00027 *                                            49-54 KENDALL COUNTY
00028 *                                            EQUALIZED VALUATION
00029      05  AEV-LSL-EQV       PIC 9(11)    PACKED-DECIMAL.
00030 *                                            55-60 LASALLE COUNTY
00031 *                                            EQUALIZED VALUATION
00032      05  AEV-MCH-EQV       PIC 9(11)    PACKED-DECIMAL.
00033 *                                            61-66 MCHENRY COUNTY
00034 *                                            EQUALIZED VALUATION
00035      05  AEV-GRN-EQV       PIC 9(11)    PACKED-DECIMAL.
00036 *                                            67-72 GRUNDY COUNTY
00037 *                                            EQUALIZED VALUATION
00038      05  AEV-DKB-EQV       PIC 9(11)    PACKED-DECIMAL.
00039 *                                            73-78 DEKALB COUNTY
00040 *                                            EQUALIZED VALUATION
00041      05  AEV-LVN-EQV       PIC 9(11)    PACKED-DECIMAL.
00042 *                                            79-84 LIVINGSTON
00043 *                                                  COUNTY
00044 *                                            EQUALIZED VALUATION
00045      05  AEV-KNE-EQV       PIC 9(11)    PACKED-DECIMAL.
00046 *                                            85-90 KANE COUNTY
00047 *                                            EQUALIZED VALUATION
00048      05  AEV-WIL-EQV       PIC 9(11)    PACKED-DECIMAL.
00049 *                                            91-96 WILL COUNTY
00050 *                                            EQUALIZED VALUATION
00051      05  AEV-PAR-AGY1      PIC 9(9)     PACKED-DECIMAL.
00052 *                                            97-101 PARENT AGENCY
00053 *                                                  ONE
00054      05  AEV-PAR-AGY2      PIC 9(9)     PACKED-DECIMAL.
00055 *                                           102-106 PARENT AGENCY
00056 *                                                  TWO
00057      05  AEV-PAR-AGY3      PIC 9(9)     PACKED-DECIMAL.
00058 *                                            107-111 PARENT AGENCY
00059 *                                                  THREE
00060      05  AEV-PAR-AGY4      PIC 9(9)     PACKED-DECIMAL.
00061 *                                            112-116 PARENT AGENCY
00062 *                                                  FOUR
00063      05  AEV-PAR-AGY5      PIC 9(9)     PACKED-DECIMAL.
00064 *                                            117-121 PARENT AGENCY
00065 *                                                  FIVE
00066      05  AEV-PCT-BURDEN    PIC 999V99   PACKED-DECIMAL.
00067 *                                            122-124 % OF BURDEN
00068      05  AEV-TAX-YEAR      PIC 9(5)     PACKED-DECIMAL.
00069 *                                            125-127 TAX YEAR
00070      05  AEV-TAXCAP        PIC X.
00071 *                                            128-128 TAX CAP
00072 *                                                    INDICATOR
00073      05  FILLER            PIC X(4).
00074 *                                            129-132 SPACES
00075      05  AEV-CON-AGCY1     PIC 9(9)     PACKED-DECIMAL.
00076 *                                            133-137 CONNECTING
00077 *                                                    AGENCY 1
00078      05  AEV-CON-AGCY2     PIC 9(9)     PACKED-DECIMAL.
00079 *                                            138-142 CONNECTING
00080 *                                                    AGENCY 2
00081      05  AEV-CON-AGCY3     PIC 9(9)     PACKED-DECIMAL.
00082 *                                            143-147 CONNECTING
00083 *                                                    AGENCY 3
00084      05  AEV-CON-AGCY4     PIC 9(9)     PACKED-DECIMAL.
00085 *                                            148-152 CONNECTING
00086 *                                                    AGENCY 4
00087      05  AEV-NEW-PROP-EQV  PIC 9(11)    PACKED-DECIMAL.
00088 *                                            153-158 NEW PROPERTY
00089 *                                            EQUALIZED VALUATION
00090      05  AEV-ANX-PROP-EQV  PIC 9(11)    PACKED-DECIMAL.
00091 *                                            159-164 ANNEXED PROP
00092 *                                            EQUALIZED VALUATION
00093      05  AEV-DIS-PROP-EQV  PIC 9(11)    PACKED-DECIMAL.
00094 *                                            165-170 DISCONNECTED
00095 *                                            PROPERTY
00096 *                                            EQUALIZED VALUATION
00097      05  AEV-DIS-TIF-DIF   PIC 9(11)    PACKED-DECIMAL.
00098 *                                            171-176 DISCONNECTED
00099 *                                            TIF VAL DIFFERECNE
00100      05  AEV-OVLP-NEW-PROP-EQV  PIC 9(11)    PACKED-DECIMAL.
00101 *                                            177-182 OVERLAP NEW
00102 *                                            PROPERTY EQUAL VAL
00103      05  AEV-OVLP-ANX-PROP-EQV  PIC 9(11)    PACKED-DECIMAL.
00104 *                                            183-188 OVERLAP ANNEX
00105 *                                            PROP EQUAL VALUATION
00106      05  AEV-OVLP-DIS-PROP-EQV  PIC 9(11)    PACKED-DECIMAL.
00107 *                                            189-194  OVERLAP
00108 *                                            DISCONNECTED PROPERTY
00109 *                                            EQUALIZED VALUATION
00110      05  AEV-OVLP-DIS-TIF-DIF   PIC 9(11)    PACKED-DECIMAL.
00111 *                                            195-200 OVERLAP DIS-
00112 *                                            CONNECTED TIF VAL
00113 *                                            DIFFERENCE
00114      05  AEV-PREV-TXYR1         PIC 9(05)    PACKED-DECIMAL.
00115 *                                            201-203 PREVIOUS TAX
00116 *                                            YEAR 1
00117      05  AEV-PREV-TXYR1-TX-XT   PIC 9(11)V99 PACKED-DECIMAL.
00118 *                                            204-210 PREVIOUS TAX
00119 *                                            YEAR 1 TAX EXTENSION
00120      05  AEV-PREV-TXYR2         PIC 9(05)    PACKED-DECIMAL.
00121 *                                            211-213 PREVIOUS TAX
00122 *                                            YEAR 2
00123      05  AEV-PREV-TXYR2-TX-XT   PIC 9(11)V99 PACKED-DECIMAL.
00124 *                                            214-220 PREVIOUS TAX
00125 *                                            YEAR 2 TAX EXTENSION
00126      05  AEV-PREV-TXYR3         PIC 9(05)    PACKED-DECIMAL.
00127 *                                            221-223 PREVIOUS TAX
00128 *                                            YEAR 3
00129      05  AEV-PREV-TXYR3-TX-XT   PIC 9(11)V99 PACKED-DECIMAL.
00130 *                                            224-230 PREVIOUS TAX
00131 *                                            YEAR 3 TAX EXTENSION
00132      05  AEV-LIMT-TXRTE-OVRD    PIC S9(3)V9(6) PACKED-DECIMAL.
00133 *                                            231-235 LIMITING TAX
00134 *                                            RATE OVERRIDE
00135      05  FILLER            PIC X(21).
00136 *                                            236-256 SPACES
