00001 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
00002 *           O L D,   U S E - 'ASDTLQRD03'
00003 * * D E T A I L  O R  Q U E S T I O N N A I R E  S E G M E N T * *
00004  01  D1-REC.
00005         10 D1-MULTICODE                PIC S9(3)    COMP-3.
00006         10 D1-TYPE                     PIC X.
00007         10 D1-NUMBER                   PIC X.
00008         10 D1-DECIMAL                  PIC X.
00009         10 D1-UNIT-MEASURE             PIC XX.
00010         10 D1-MAJ-MI-CLASS             PIC S9(3)    COMP-3.
00011         10 D1-EX-RR                    PIC X.
00012         10 D1-FRONT-FOOTAGE            PIC S9(7)    COMP-3.
00013         10 D1-DEPTH                    PIC S9(5)    COMP-3.
00014         10 D1-UNIT-PRICE               PIC S9(5)V99 COMP-3.
00015         10 D1-DEPTH-FACTOR             PIC S99V999  COMP-3.
00016         10 D1-CORNER-FACTOR            PIC S9V9999  COMP-3.
00017         10 D1-EX-CORNER-FACTOR         PIC SV9(5)   COMP-3.
00018         10 D1-PERCENT-ASSESSED         PIC S99V9(5) COMP-3.
00019         10 D1-BUFF-NO                  PIC S9(3)    COMP-3.
00020         10 D1-CND REDEFINES D1-BUFF-NO PIC 99V9     COMP-3.
00021         10 D1-VALUATION                PIC S9(9)    COMP-3.
00022         10 FILLER                      PIC X(11).
00023         10 D1-UNIT-IND                 PIC X.
00024  01  D2-REC REDEFINES D1-REC.
00025         10 D2-MULTICODE                PIC S9(3)    COMP-3.
00026         10 D2-TYPE                     PIC X.
00027         10 D2-NUMBER                   PIC X.
00028         10 D2-DECIMAL                  PIC X.
00029         10 D2-UNIT-MEASURE             PIC XX.
00030         10 D2-MAJ-MI-CLASS             PIC S9(3)    COMP-3.
00031         10 D2-CDU                      PIC XX.
00032         10 D2-C-D-U REDEFINES D2-CDU   PIC XX.
00033         10 D2-AREA                     PIC S9(7)    COMP-3.
00034         10 D2-A-R-E-A REDEFINES D2-AREA
00035                                        PIC S9(7)    COMP-3.
00036         10 D2-UNIT-PRICE               PIC S9(5)V99 COMP-3.
00037         10 D2-PRODUCT                  PIC S9(9)    COMP-3.
00038         10 D2-AGE                      PIC S9(3)    COMP-3.
00039         10 D2-A-G-E REDEFINES D2-AGE   PIC S9(3)    COMP-3.
00040         10 D2-CONDITION                PIC S99V9    COMP-3.
00041         10 D2-PERCENT-ASSESSED         PIC S99V9(5) COMP-3.
00042         10 D2-BUFF-NO                  PIC S9(3)    COMP-3.
00043         10 FILLER                      PIC XX.
00044         10 D2-VALUATION                PIC S9(9)    COMP-3.
00045         10 D2-KEYPARCEL                PIC 9(15)    COMP-3.
00046         10 D2-SPLITCODE                PIC X.
00047         10 FILLER                      PIC X(3).
00048  01  D3-REC REDEFINES D1-REC.
00049         10 D3-MULTICODE                PIC S9(3)    COMP-3.
00050         10 D3-TYPE                     PIC X.
00051         10 D3-NUMBER                   PIC X.
00052         10 FILLER                      PIC XXX.
00053         10 D3-MAJ-MI-CLASS             PIC S9(3)    COMP-3.
00054         10 D3-CDU                      PIC XX.
00055         10 D3-C-D-U REDEFINES D3-CDU   PIC XX.
00056         10 D3-REPROD-COST              PIC S9(9)    COMP-3.
00057         10 FILLER                      PIC X(6).
00058         10 D3-IMPV-YR                  PIC 9(3)     COMP-3.
00059         10 D3-AGE                      PIC S9(3)    COMP-3.
00060         10 D3-A-G-E REDEFINES D3-AGE   PIC S9(3)    COMP-3.
00061         10 D3-CONDITION                PIC S99V9    COMP-3.
00062         10 D3-PERCENT-ASSESSED         PIC S99V9(5) COMP-3.
00063         10 D3-BUFF-NO                  PIC S9(3)    COMP-3.
00064         10 FILLER                      PIC XX.
00065         10 D3-VALUATION                PIC S9(9)    COMP-3.
00066         10 D3-KEYPARCEL                PIC 9(15)    COMP-3.
00067         10 D3-SPLITCODE                PIC X.
00068         10 FILLER                      PIC X(3).
00069  01  D4-REC REDEFINES D1-REC.
00070         10 D4-MULTICODE                PIC S9(3)    COMP-3.
00071         10 D4-TYPE                     PIC X.
00072         10 D4-NUMBER                   PIC X.
00073         10 FILLER                      PIC X(3).
00074         10 D4-MAJ-MI-CLASS             PIC S9(3)    COMP-3.
00075         10 D4-CDU                      PIC XX.
00076         10 D4-C-D-U REDEFINES D4-CDU   PIC XX.
00077         10 D4-REPROD-COST              PIC S9(9)    COMP-3.
00078         10 D4-TOT-VALUE                PIC S9(9)    COMP-3.
00079         10 FILLER                      PIC X.
00080         10 D4-OCCP-FACTOR              PIC S99V9    COMP-3.
00081         10 D4-AGE                      PIC S9(3)    COMP-3.
00082         10 D4-A-G-E REDEFINES D4-AGE   PIC S9(3)    COMP-3.
00083         10 D4-CONDITION                PIC S99V9    COMP-3.
00084         10 D4-PERCENT-ASSESSED         PIC S99V9(5) COMP-3.
00085         10 D4-BUFF-NO                  PIC S9(3)    COMP-3.
00086         10 FILLER                      PIC XX.
00087         10 D4-VALUATION                PIC S9(9)    COMP-3.
00088         10 D4-KEYPARCEL                PIC 9(15)    COMP-3.
00089         10 D4-SPLITCODE                PIC X.
00090         10 FILLER                      PIC X(3).
00091  01  D5-REC REDEFINES D1-REC.
00092         10 D5-MULTICODE                PIC S9(3)    COMP-3.
00093         10 D5-TYPE                     PIC X.
00094         10 D5-NUMBER                   PIC X.
00095         10 FILLER                      PIC X(3).
00096         10 D5-MAJ-MI-CLASS             PIC S9(3)    COMP-3.
00097         10 D5-CDU                      PIC XX.
00098         10 D5-C-D-U REDEFINES D5-CDU   PIC XX.
00099         10 D5-REPROD-COST              PIC S9(9)    COMP-3.
00100         10 FILLER                      PIC X(6).
00101         10 D5-OCCP-FACTOR              PIC S99V9    COMP-3.
00102         10 D5-AGE                      PIC S9(3)    COMP-3.
00103         10 D5-A-G-E REDEFINES D5-AGE   PIC S9(3)    COMP-3.
00104         10 D5-CONDITION                PIC S99V9    COMP-3.
00105         10 D5-PERCENT-ASSESSED         PIC S99V9(5) COMP-3.
00106         10 D5-BUFF-NO                  PIC S9(3)    COMP-3.
00107         10 FILLER                      PIC XX.
00108         10 D5-VALUATION                PIC S9(9)    COMP-3.
00109         10 D5-KEYPARCEL                PIC 9(15)    COMP-3.
00110         10 D5-SPLITCODE                PIC X.
00111         10 FILLER                      PIC X(3).
00112  01  QST-REC REDEFINES D1-REC.
00113       05 QFILL1.
00114           10 Q1                        PIC 9.
00115           10 Q2                        PIC 9.
00116           10 Q2A                       PIC 9.
00117           10 Q3                        PIC 9.
00118           10 Q4                        PIC 9.
00119       05 QFILL2.
00120           10 Q5A1                      PIC S9(3) COMP-3.
00121       05 QFILL3.
00122           10 Q5A2                      PIC 99.
00123           10 Q5B1                      PIC 9.
00124           10 Q5B2                      PIC 9.
00125           10 Q5C1                      PIC 9.
00126           10 Q5C2F                     PIC 9.
00127           10 Q5C2U                     PIC 9.
00128           10 Q5C2S                     PIC 9.
00129           10 Q5C2O                     PIC 9.
00130           10 Q5C3                      PIC 9.
00131           10 Q5C4                      PIC 9.
00132           10 Q5C41                     PIC 9.
00133           10 Q5C42                     PIC 9.
00134           10 Q5C43                     PIC 9.
00135           10 Q5C4B                     PIC 9.
00136           10 Q5D1                      PIC 9.
00137           10 Q5D2                      PIC 9.
00138           10 Q5E1                      PIC 99.
00139           10 Q5E2                      PIC 9.
00140           10 Q6A                       PIC 9.
00141           10 Q6B                       PIC 9.
00142           10 Q7A                       PIC 9.
00143           10 Q7B                       PIC 9.
00144           10 Q8                        PIC 9.
00145           10 Q9A                       PIC 9.
00146           10 Q9B                       PIC 9.
00147           10 Q9C                       PIC 9.
00148           10 Q9D                       PIC 9.
00149           10 Q10A                      PIC 9.
00150           10 Q10B                      PIC 9.
00151           10 Q10C                      PIC 9.
00152           10 Q10D                      PIC 9.
00153           10 Q11                       PIC 9.
00154       05 QFILL4.
00155           10 Q12                       PIC S9(7) COMP-3.
00156           10 Q13                       PIC S9(7) COMP-3.
00157       05 QFILL5.
00158           10 Q16                       PIC 9.
00159           10 Q17                       PIC 9.
00160       05 QFILL6.
00161           10 FILLER                    PIC XX.
00162 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
