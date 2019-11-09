00001 *     --CREATE STATE REPORT CLASS FILE--   *BYTES* *DESCRIPTION  *
00002      05  FV-KEY.
00003 *                                            1-8    KEY
00004          10  FV-DIVNO          PIC 9(14)    COMP-3.
00005 *                                            1-8      TAX CODE
00006      05  FV-VALUES.
00007          10  FV-VAL OCCURS 29 TIMES PIC S9(13) COMP-3.
00008 *                                            9-211  SALES REPORT
00009 *                                                     VALUES
00010      05  FILLER REDEFINES FV-VALUES.
00011          10  FV-PRIOR-VALUES.
00012              15  FV-PRIOR-VAL OCCURS 4 TIMES PIC S9(13) COMP-3.
00013 *                                            9-36   PRIOR VALUES
00014          10  FV-CURRENT-VALUES.
00015              15  FV-CURRENT-VAL OCCURS 4 TIMES PIC S9(13) COMP-3.
00016 *                                           37-64   CURRENT VALUES
00017          10  FV-PROPOSED-VALUES.
00018              15  FV-PROPOSED-VAL OCCURS 5 TIMES PIC S9(13) COMP-3.
00019 *                                           65-99   PROPOSED VALUE
00020          10  FV-PARWCA-PRIOR-VALUES.
00021              15  FV-PARWCA-PRIOR-VAL OCCURS 4 TIMES
00022                                                PIC S9(13) COMP-3.
00023 *                                          100-127  PARCELS WITH
00024 *                                                   CHG ACTION
00025 *                                                   PRIOR VALUES
00026          10  FV-PARWCA-CURRENT-VALUES.
00027              15  FV-PARWCA-CURRENT-VAL OCCURS 4 TIMES
00028                                                PIC S9(13) COMP-3.
00029 *                                          128-155  PARCELS WITH
00030 *                                                   CHG ACTION
00031 *                                                   CURRENT VALUES
00032          10  FV-PARWOCA-PRIOR-VALUES.
00033              15  FV-PARWOCA-PRIOR-VAL OCCURS 4 TIMES
00034                                                PIC S9(13) COMP-3.
00035 *                                          156-183  PARCELS WITHOU
00036 *                                                   CHG ACTION
00037 *                                                   PRIOR VALUES
00038          10  FV-PARWOCA-CURRENT-VALUES.
00039              15  FV-PARWOCA-CURRENT-VAL OCCURS 4 TIMES
00040                                                PIC S9(13) COMP-3.
00041 *                                          184-211  PARCELS WITHOU
00042 *                                                   CHG ACTION
00043 *                                                   CURRENT VALUES
00044      05  FILLER REDEFINES FV-VALUES.
00045          10  FV-PRIOR-LND-VAL        PIC S9(13) COMP-3.
00046 *                                            9-15
00047 *                                                   PRIOR LAND
00048 *                                                     VALUE
00049          10  FV-PRIOR-IMP-VAL        PIC S9(13) COMP-3.
00050 *                                           16-22
00051 *                                                   PRIOR IMP
00052 *                                                     VALUE
00053          10  FV-PRIOR-TOT-VAL        PIC S9(13) COMP-3.
00054 *                                           23-29
00055 *                                                   PRIOR TOTAL
00056 *                                                     VALUE
00057          10  FV-PRIOR-PCL            PIC S9(13) COMP-3.
00058 *                                           30-36
00059 *                                                   PRIOR PARCEL
00060 *                                                     COUNT
00061          10  FV-CURR-LND-VAL         PIC S9(13) COMP-3.
00062 *                                           37-43
00063 *                                                   CURRENT LAND
00064 *                                                     VALUE
00065          10  FV-CURR-IMP-VAL         PIC S9(13) COMP-3.
00066 *                                           44-50
00067 *                                                   CURRENT IMP
00068 *                                                     VALUE
00069          10  FV-CURR-TOT-VAL         PIC S9(13) COMP-3.
00070 *                                           51-57
00071 *                                                   CURRENT TOT
00072 *                                                     VALUE
00073          10  FV-CURR-PCL             PIC S9(13) COMP-3.
00074 *                                           58-64
00075 *                                                   CURRENT PARCEL
00076 *                                                     COUNT
00077          10  FV-PROP-IMP-VAL         PIC S9(13) COMP-3.
00078 *                                           65-71
00079 *                                                   PROPOSED IMP
00080 *                                                     VALUE
00081          10  FV-PROP-EXPR-288-VAL    PIC S9(13) COMP-3.
00082 *                                           72-78
00083 *                                                   PROPOSED
00084 *                                                     EXPIRED 288
00085 *                                                     VALUE
00086          10  FV-PROP-CURR-288-VAL    PIC S9(13) COMP-3.
00087 *                                           79-85
00088 *                                                   PROPOSED
00089 *                                                     CURRENT 288
00090 *                                                     VALUE
00091          10  FV-PROP-TOT-VAL         PIC S9(13) COMP-3.
00092 *                                           86-92
00093 *                                                   PROPOSED
00094 *                                                     TOT VALUE
00095          10  FV-PROP-ACT-VAL         PIC S9(13) COMP-3.
00096 *                                           93-99
00097 *                                                   PROPOSED
00098 *                                                     ACTUAL VALUE
00099          10  FV-CA-PR-LND-VAL        PIC S9(13) COMP-3.
00100 *                                          100-106  CHG ACTION
00101 *                                                     PRIOR LND
00102 *                                                     VALUE
00103          10  FV-CA-PR-IMP-VAL        PIC S9(13) COMP-3.
00104 *                                          107-113  CHG ACTION
00105 *                                                     PRIOR IMP
00106 *                                                     VALUE
00107          10  FV-CA-PR-TOT-VAL        PIC S9(13) COMP-3.
00108 *                                          114-120  CHG ACTION
00109 *                                                     PRIOR TOT
00110 *                                                     VALUE
00111          10  FV-CA-PR-PCL            PIC S9(13) COMP-3.
00112 *                                          121-127  CHG ACTION
00113 *                                                     PRIOR PCL
00114 *                                                     COUNT
00115          10  FV-CA-CR-LND-VAL        PIC S9(13) COMP-3.
00116 *                                          128-134  CHG ACTION
00117 *                                                     CURRENT LND
00118 *                                                     VALUE
00119          10  FV-CA-CR-IMP-VAL        PIC S9(13) COMP-3.
00120 *                                          135-141  CHG ACTION
00121 *                                                     CURRENT IMP
00122 *                                                     VALUE
00123          10  FV-CA-CR-TOT-VAL        PIC S9(13) COMP-3.
00124 *                                          142-148  CHG ACTION
00125 *                                                     CURRENT TOT
00126 *                                                     VALUE
00127          10  FV-CA-CR-PCL            PIC S9(13) COMP-3.
00128 *                                          149-155  CHG ACTION
00129 *                                                     CURRENT PCL
00130 *                                                     COUNT
00131          10  FV-WOCA-PR-LND-VAL      PIC S9(13) COMP-3.
00132 *                                          156-162  NO CHG ACTION
00133 *                                                     PRIOR LND
00134 *                                                     VALUE
00135          10  FV-WOCA-PR-IMP-VAL      PIC S9(13) COMP-3.
00136 *                                          163-169  NO CHG ACTION
00137 *                                                     PRIOR IMP
00138 *                                                     VALUE
00139          10  FV-WOCA-PR-TOT-VAL      PIC S9(13) COMP-3.
00140 *                                          170-176  NO CHG ACTION
00141 *                                                     PRIOR TOT
00142 *                                                     VALUE
00143          10  FV-WOCA-PR-PCL          PIC S9(13) COMP-3.
00144 *                                          177-183  NO CHG ACTION
00145 *                                                     PRIOR PCL
00146 *                                                     COUNT
00147          10  FV-WOCA-CR-LND-VAL      PIC S9(13) COMP-3.
00148 *                                          184-190  NO CHG ACTION
00149 *                                                     CURRENT LND
00150 *                                                     VALUE
00151          10  FV-WOCA-CR-IMP-VAL      PIC S9(13) COMP-3.
00152 *                                          191-197  NO CHG ACTION
00153 *                                                     CURRENT IMP
00154 *                                                     VALUE
00155          10  FV-WOCA-CR-TOT-VAL      PIC S9(13) COMP-3.
00156 *                                          198-204  NO CHG ACTION
00157 *                                                     CURRENT TOT
00158 *                                                     VALUE
00159          10  FV-WOCA-CR-PCL          PIC S9(13) COMP-3.
00160 *                                          205-211  NO CHG ACTION
00161 *                                                     CURRENT PCL
00162 *                                                     COUNT
00163      05  FILLER                      PIC X(19).
00164 *                                          212-230  FILLER
00165 *----------------------------------------------------------------*
