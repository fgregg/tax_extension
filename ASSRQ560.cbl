00001  IDENTIFICATION DIVISION.
00002  PROGRAM-ID. ASSRQ560.
00003  AUTHOR. JENDRAS - MOLIS.
00004  DATE-WRITTEN. JUL 15, 1992.
00005 *REMARKS. THIS PROGRAM PRINTS THE ASSESSMENT RECORDS IN THE
00006 *         CHICAGO CIRCULATOR WHICH WILL BE TAXED AND HAVE
00007 *         MIXED MAJOR CLASSES, AND APPLIES TAX TYPES TO THE
00008 *         ASSESSMENT MASTER FILES.
00009 *9/13  COMMENTED OUT CIRCULATOR LOGIC FOR TAX YEAR 2013 PER
00010 *      BILL V. COUNTY CLERK'S OFFICE
00011  ENVIRONMENT DIVISION.
00012  CONFIGURATION SECTION.
00013  SOURCE-COMPUTER. IBM-370.
00014  OBJECT-COMPUTER. IBM-370.
00015  INPUT-OUTPUT SECTION.
00016  FILE-CONTROL.
00017      SELECT MASTER-IN ASSIGN TO UT-S-MASTIN.
00018      SELECT MASTER-OUT-2 ASSIGN TO UT-S-MASTOUT.
00019      SELECT PRINT-FILE ASSIGN TO UT-S-PRINT.
00020  DATA DIVISION.
00021  FILE SECTION.
00022  COPY ASREASFD01.
00023  COPY ASREASFD03.
00024  COPY PRINTFILE.
00025  WORKING-STORAGE SECTION.
00026  77  AS-MAST-RECS       PIC S9(7)     VALUE +0  PACKED-DECIMAL.
00027  77  RECS-PRINTED       PIC S9(7)     VALUE +0  PACKED-DECIMAL.
00028  77  RECS-WRITTEN       PIC S9(7)     VALUE +0  PACKED-DECIMAL.
00029  77  LINE-CNTR          PIC S9(3)     VALUE +60 PACKED-DECIMAL.
00030  77  PAGE-CNTR          PIC S9(5)     VALUE +0  PACKED-DECIMAL.
00031  77  TOTAL-LAND         PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00032  77  TOTAL-IMPRV        PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00033  77  TOTAL-TOTAL        PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00034  77  TOTAL-REC-LAND     PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00035  77  TOTAL-REC-IMPRV    PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00036  77  TOTAL-REC-VALUE    PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00037  77  TOTAL-TAX-LAND     PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00038  77  TOTAL-TAX-IMPRV    PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00039  77  TOTAL-TAX-VALUE    PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00040  77  TOTAL-NON-LAND     PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00041  77  TOTAL-NON-IMPRV    PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00042  77  TOTAL-NON-VALUE    PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00043  77  SUB                PIC S9(4)     VALUE +0  BINARY.
00044  77  BLANK-LINE         PIC X         VALUE SPACE.
00045  77  AS-EOF-SW          PIC X         VALUE 'N'.
00046      88  AS-EOF                       VALUE 'Y'.
00047  77  SELECT-REC-SW      PIC X         VALUE 'N'.
00048      88  SELECTED                     VALUE 'Y'.
00049  77  COND-1-SW          PIC X         VALUE 'N'.
00050      88  COND-1                       VALUE 'Y'.
00051  77  COND-2-SW          PIC X         VALUE 'N'.
00052      88  COND-2                       VALUE 'Y'.
00053  77  TYPE-AS-SW         PIC X         VALUE 'N'.
00054      88  TYPE1                        VALUE '1'.
00055      88  TYPE2-5                      VALUE '2'.
00056  77  CLASS-CHECK        PIC 999.
00057      88  CLASS-100-199                VALUE 100 THRU 199.
00058      88  CLASS-200-399                VALUE 200 THRU 399.
00059      88  CLASS-400-999                VALUE 400 THRU 999.
00060      88  CLASS-600-699                VALUE 600 THRU 699.
00061  01  WS-TXCD            PIC 9(5).
00062  01  FILLER REDEFINES WS-TXCD.
00063      05  FILLER         PIC XX.
00064      05  WS-TXCDHO      PIC 9.
00065      05  FILLER         PIC XX.
00066  01  WORK-AREA.
00067      05  DSP-DATE.
00068          10  DSP-MO                 PIC 99.
00069          10  FILLER                 PIC X         VALUE '/'.
00070          10  DSP-DA                 PIC 99.
00071          10  FILLER                 PIC X         VALUE '/'.
00072          10  DSP-YR                 PIC 99.
00073      05  ACPT-DATE                  PIC 9(6).
00074      05  ACPT-DATE-X REDEFINES ACPT-DATE.
00075          10  ACPT-YR                PIC 99.
00076          10  ACPT-MO                PIC 99.
00077          10  ACPT-DA                PIC 99.
00078      05  HEADING1.
00079          10  FILLER          PIC XX.
00080          10  HD1-DATE        PIC X(8).
00081          10  FILLER          PIC X(40)   VALUE SPACES.
00082          10  FILLER          PIC X(68)   VALUE
00083              'OFFICE OF THE COOK COUNTY ASSESSOR'.
00084          10  FILLER          PIC X(5)    VALUE 'PAGE'.
00085          10  HD1-PAGE        PIC ZZ,ZZ9.
00086      SKIP1
00087      05  HEADING2.
00088          10  FILLER          PIC X(41)   VALUE '  ASSRQ560'.
00089          10  FILLER          PIC X(42)   VALUE
00090              '        PARCELS IN CHICAGO CIRCULATOR AREA'.
00091          10  FILLER          PIC X(36)   VALUE
00092              ' - MIXED RESIDENTIAL/NON-RESIDENTIAL'.
00093      SKIP1
00094      05  HEADING3.
00095          10  FILLER          PIC X(54)   VALUE
00096              ' VOL      PERMANENT         TAX  OVER-ALL   LAND'.
00097          10  FILLER          PIC X(19)   VALUE
00098              'IMPROVMNT     TOTAL'.
00099      SKIP1
00100      05  HEADING4.
00101          10  FILLER          PIC X(9).
00102          10  FILLER          PIC X(45)   VALUE
00103              'INDEX NUMBER      CODE   CLASS   VALUATION'.
00104          10  FILLER          PIC X(53)   VALUE
00105           'VALUATION   VALUATION   TYPE  CLASS  CDU    VALUATION'.
00106      SKIP1
00107      05  DETAIL-LINE                     VALUE SPACES.
00108          10  FILLER          PIC X.
00109          10  D-VOL           PIC 999.
00110          10  FILLER          PIC XX.
00111          10  D-PROP          PIC 99,99,999,999,9999.
00112          10  FILLER          PIC XX.
00113          10  D-TAXCD         PIC 99999.
00114          10  FILLER          PIC XXXX.
00115          10  D-CLASS         PIC 9,99.
00116          10  FILLER          PIC XX.
00117          10  D-VALUE-L       PIC ZZZ,ZZZ,ZZ9.
00118          10  FILLER          PIC X.
00119          10  D-VALUE-I       PIC ZZZ,ZZZ,ZZ9.
00120          10  FILLER          PIC X.
00121          10  D-VALUE-T       PIC ZZZ,ZZZ,ZZ9.
00122          10  FILLER          PIC XX.
00123          10  D-TYPE          PIC X(4).
00124          10  FILLER          PIC XXX.
00125          10  D-CLASS-DET     PIC 9,99.
00126          10  FILLER          PIC XXX.
00127          10  D-CDU           PIC XX.
00128          10  FILLER          PIC XXX.
00129          10  D-VALUE         PIC ZZZ,ZZZ,ZZ9.
00130          SKIP1
00131      05  TOTAL-MSG-LINE                  VALUE SPACES.
00132          10  FILLER          PIC X(5).
00133          10  T-MSG           PIC X(25).
00134          10  T-CNTR          PIC ZZZ,ZZ9.
00135          10  FILLER          PIC X(27).
00136          10  T-MSG-2         PIC X(29).
00137          10  T-CNTR-2        PIC ZZZ,ZZZ,ZZZ,ZZ9.
00138          SKIP1
00139      05  TOTAL-MSG-LINE-2                VALUE SPACES.
00140          10  FILLER          PIC X(26).
00141          10  T-MSG-2V        PIC X(51).
00142          10  T-CNTR-2V       PIC ZZZ,ZZZ,ZZZ,ZZ9.
00143  PROCEDURE DIVISION.
00144  000-HOUSEKEEPING.
00145      OPEN INPUT MASTER-IN
00146          OUTPUT MASTER-OUT-2 PRINT-FILE
00147      ACCEPT ACPT-DATE FROM DATE
00148      MOVE ACPT-MO TO DSP-MO
00149      MOVE ACPT-DA TO DSP-DA
00150      MOVE ACPT-YR TO DSP-YR
00151      MOVE DSP-DATE TO HD1-DATE
00152      WRITE PRINT-REC FROM BLANK-LINE AFTER ADVANCING PAGE
00153      PERFORM 100-MAINLINE UNTIL AS-EOF
00154      PERFORM 800-END-RTN
00155      CLOSE MASTER-IN MASTER-OUT-2 PRINT-FILE
00156      STOP RUN.
00157  100-MAINLINE.
00158      PERFORM 200-READ-AS-MAST
00159      IF NOT AS-EOF
00160 *9/13 COMMENTED OUT CIRCULATOR LOGIC TXYR 2013 PER BILL V CCCO
00161 *        IF M-TXCD NOT = 74004 AND 74504 AND 76010 AND 76510
00162 *            AND 76011 AND 76511 AND 76012 AND 76512
00163 *            AND 76013 AND 76513 AND 76014 AND 76514
00164 *            AND 76015 AND 76515 AND 76016 AND 76516
00165 *            AND 77004 AND 77504 AND 77005 AND 77505
00166                  PERFORM 600-MOVE-MASTER-TO-OUTPUT
00167                  MOVE '0' TO MA-TXTYP
00168                  PERFORM 700-WRITE-MASTER.
00169 *        ELSE
00170 *            MOVE 'N' TO SELECT-REC-SW COND-1-SW COND-2-SW
00171 *            PERFORM 250-EVALUATE-AS-DETAIL
00172 *                VARYING SUB FROM 1 BY 1
00173 *                UNTIL SUB > M-DTL-QST-CTR
00174 *            IF SELECTED
00175 *                MOVE M-BASE TO MA-BASE
00176 *                MOVE '0' TO MA-TXTYP
00177 *                PERFORM 375-MOVE-SALES-SEGMENT
00178 *                MOVE 0 TO MA-DTL-QST-CTR-1
00179 *                    TOTAL-REC-LAND TOTAL-REC-IMPRV
00180 *                    TOTAL-REC-VALUE
00181 *                PERFORM 380-CREATE-COND-1-SEGS
00182 *                    VARYING SUB FROM 1 BY 1
00183 *                    UNTIL SUB > M-DTL-QST-CTR
00184 *                PERFORM 300-PRINT-AS-DETAILS
00185 *                    VARYING SUB FROM 1 BY 1
00186 *                    UNTIL SUB > MA-DTL-QST-CTR-1
00187 *                MOVE TOTAL-REC-LAND TO MA-VALUE-1 (7)
00188 *                MOVE TOTAL-REC-IMPRV TO MA-VALUE-1 (8)
00189 *                MOVE TOTAL-REC-VALUE TO MA-VALUE-1 (9)
00190 *                PERFORM 350-MOVE-PRINT-FIXED-DETAIL
00191 *                PERFORM 700-WRITE-MASTER
00192 *                MOVE M-BASE TO MA-BASE
00193 *                MOVE MA-TXCD TO WS-TXCD
00194 *                MOVE 5 TO WS-TXCDHO
00195 *                MOVE WS-TXCD TO MA-TXCD
00196 *                MOVE '5' TO MA-TXTYP
00197 *                PERFORM 375-MOVE-SALES-SEGMENT
00198 *                MOVE 0 TO MA-DTL-QST-CTR-1
00199 *                    TOTAL-REC-LAND TOTAL-REC-IMPRV
00200 *                    TOTAL-REC-VALUE
00201 *                PERFORM 390-CREATE-COND-2-SEGS
00202 *                    VARYING SUB FROM 1 BY 1
00203 *                    UNTIL SUB > M-DTL-QST-CTR
00204 *                PERFORM 300-PRINT-AS-DETAILS
00205 *                    VARYING SUB FROM 1 BY 1
00206 *                    UNTIL SUB > MA-DTL-QST-CTR-1
00207 *                MOVE TOTAL-REC-LAND TO MA-VALUE-1 (7)
00208 *                MOVE TOTAL-REC-IMPRV TO MA-VALUE-1 (8)
00209 *                MOVE TOTAL-REC-VALUE TO MA-VALUE-1 (9)
00210 *                PERFORM 350-MOVE-PRINT-FIXED-DETAIL
00211 *                PERFORM 700-WRITE-MASTER
00212 *            ELSE
00213 *                PERFORM 600-MOVE-MASTER-TO-OUTPUT
00214 *                MOVE '0' TO MA-TXTYP
00215 *                PERFORM 700-WRITE-MASTER.
00216  200-READ-AS-MAST.
00217      READ MASTER-IN
00218          AT END MOVE 'Y' TO AS-EOF-SW.
00219      IF NOT AS-EOF
00220          ADD +1 TO AS-MAST-RECS.
00221  250-EVALUATE-AS-DETAIL.
00222      IF D1-TYPE1 (SUB) OR D2-TYPE2-5 (SUB)
00223          MOVE D1-CLS (SUB) TO CLASS-CHECK
00224          IF (CLASS-100-199)
00225              MOVE 'Y' TO COND-1-SW
00226          ELSE
00227              IF (CLASS-400-999) AND (D2-CDU (SUB) NOT = 'AP')
00228                  MOVE 'Y' TO COND-1-SW
00229              ELSE
00230                  IF (CLASS-200-399)
00231                      MOVE 'Y' TO COND-2-SW
00232                  ELSE
00233                      IF (CLASS-600-699)
00234                          AND (D2-CDU (SUB) EQUAL 'AP')
00235                          MOVE 'Y' TO COND-2-SW.
00236      IF COND-1 AND COND-2
00237          MOVE 'Y' TO SELECT-REC-SW.
00238      IF D2-QUES-CLASS (SUB)
00239          ADD +1 TO SUB.
00240  300-PRINT-AS-DETAILS.
00241      MOVE 'N' TO TYPE-AS-SW
00242      IF D1M-TYPE1 (SUB)
00243          MOVE '1' TO TYPE-AS-SW
00244      ELSE
00245          IF D2M-TYPE2-5 (SUB)
00246              MOVE '2' TO TYPE-AS-SW.
00247      IF TYPE1 OR TYPE2-5
00248          PERFORM 400-MOVE-SEGS-DETAIL.
00249      IF D2M-QUES-CLASS (SUB)
00250          ADD +1 TO SUB.
00251  350-MOVE-PRINT-FIXED-DETAIL.
00252      MOVE SPACES TO DETAIL-LINE
00253      MOVE MA-VOL TO D-VOL
00254      MOVE MA-PROP TO D-PROP
00255      INSPECT D-PROP REPLACING ALL ',' BY '-'
00256      MOVE MA-TXCD TO D-TAXCD
00257      MOVE MA-CLS TO D-CLASS
00258      INSPECT D-CLASS REPLACING ALL ',' BY '-'
00259      MOVE MA-VALUE-1 (7) TO D-VALUE-L
00260      ADD MA-VALUE-1 (7) TO TOTAL-LAND
00261      MOVE MA-VALUE-1 (8) TO D-VALUE-I
00262      ADD MA-VALUE-1 (8) TO TOTAL-IMPRV
00263      MOVE MA-VALUE-1 (9) TO D-VALUE-T
00264      ADD MA-VALUE-1 (9) TO TOTAL-TOTAL
00265      PERFORM 450-PRINT-DETAIL-FIX
00266      ADD +1 TO RECS-PRINTED.
00267  375-MOVE-SALES-SEGMENT.
00268      IF M-SALES-PRESENT
00269          MOVE 1 TO MA-SLS-CTR-1
00270          MOVE M-SALES (1) TO MA-SALES-1 (1).
00271  380-CREATE-COND-1-SEGS.
00272      MOVE D1-CLS (SUB) TO CLASS-CHECK
00273      IF (CLASS-100-199)
00274          OR (CLASS-400-999) AND (D2-CDU (SUB) NOT = 'AP')
00275          ADD +1 TO MA-DTL-QST-CTR-1
00276          MOVE D-TYP1 (SUB) TO DM-TYP1 (MA-DTL-QST-CTR-1)
00277          IF D2-QUES-CLASS (SUB)
00278              ADD +1 TO SUB MA-DTL-QST-CTR-1
00279              MOVE D-TYP1 (SUB) TO DM-TYP1 (MA-DTL-QST-CTR-1).
00280  390-CREATE-COND-2-SEGS.
00281      MOVE D1-CLS (SUB) TO CLASS-CHECK
00282      IF (CLASS-200-399)
00283          OR (CLASS-600-699) AND (D2-CDU (SUB) = 'AP')
00284          ADD +1 TO MA-DTL-QST-CTR-1
00285          MOVE D-TYP1 (SUB) TO DM-TYP1 (MA-DTL-QST-CTR-1)
00286          IF D2-QUES-CLASS (SUB)
00287              ADD +1 TO SUB MA-DTL-QST-CTR-1
00288              MOVE D-TYP1 (SUB) TO DM-TYP1 (MA-DTL-QST-CTR-1).
00289  400-MOVE-SEGS-DETAIL.
00290      MOVE SPACES TO DETAIL-LINE
00291      IF TYPE1
00292          MOVE 'LAND' TO D-TYPE
00293      ELSE
00294          IF TYPE2-5
00295              MOVE 'IMPV' TO D-TYPE
00296              MOVE D2M-CDU (SUB) TO D-CDU.
00297      MOVE D1M-CLS (SUB) TO D-CLASS-DET
00298      INSPECT D-CLASS-DET REPLACING ALL ',' BY '-'
00299      MOVE D1M-VAL (SUB) TO D-VALUE
00300      MOVE D1M-CLS (SUB) TO CLASS-CHECK
00301      IF D1M-TYPE1 (SUB)
00302          IF CLASS-100-199 OR CLASS-400-999
00303              ADD D1M-VAL (SUB) TO TOTAL-TAX-LAND TOTAL-TAX-VALUE
00304                  TOTAL-REC-LAND TOTAL-REC-VALUE
00305          ELSE
00306              ADD D1M-VAL (SUB) TO TOTAL-NON-LAND TOTAL-NON-VALUE
00307                  TOTAL-REC-LAND TOTAL-REC-VALUE
00308      ELSE
00309          IF D2M-TYPE2-5 (SUB)
00310              IF CLASS-100-199
00311                  OR  (CLASS-400-999
00312                      AND (D2M-CDU (SUB) NOT EQUAL 'AP'))
00313                      ADD D2M-VAL (SUB) TO TOTAL-TAX-IMPRV
00314                          TOTAL-TAX-VALUE TOTAL-REC-IMPRV
00315                          TOTAL-REC-VALUE
00316              ELSE
00317                  ADD D2M-VAL (SUB) TO TOTAL-NON-IMPRV
00318                      TOTAL-NON-VALUE TOTAL-REC-IMPRV
00319                      TOTAL-REC-VALUE.
00320      PERFORM 500-PRINT-DETAIL-SEG.
00321  450-PRINT-DETAIL-FIX.
00322      IF LINE-CNTR > +55
00323          PERFORM 900-HEADING-RTN.
00324      WRITE PRINT-REC FROM DETAIL-LINE AFTER ADVANCING 2
00325      ADD +2 TO LINE-CNTR.
00326  500-PRINT-DETAIL-SEG.
00327      IF LINE-CNTR > +55
00328          PERFORM 900-HEADING-RTN.
00329      WRITE PRINT-REC FROM DETAIL-LINE AFTER ADVANCING 1
00330      ADD +1 TO LINE-CNTR.
00331  600-MOVE-MASTER-TO-OUTPUT.
00332      MOVE M-SLS-CTR TO MA-SLS-CTR-1
00333      MOVE M-DTL-QST-CTR TO MA-DTL-QST-CTR-1
00334      MOVE MSTR-IN-REC TO MSTR-OUT-REC-2.
00335  700-WRITE-MASTER.
00336      WRITE MSTR-OUT-REC-2
00337      ADD +1 TO RECS-WRITTEN.
00338  800-END-RTN.
00339      IF LINE-CNTR > +55
00340          PERFORM 900-HEADING-RTN.
00341      MOVE SPACES TO TOTAL-MSG-LINE
00342      MOVE 'TOTAL RECORDS READ' TO T-MSG
00343      MOVE AS-MAST-RECS TO T-CNTR
00344      MOVE 'TOTAL LAND VALUATION' TO T-MSG-2
00345      MOVE TOTAL-LAND TO T-CNTR-2
00346      WRITE PRINT-REC FROM TOTAL-MSG-LINE AFTER ADVANCING 2
00347      MOVE SPACES TO TOTAL-MSG-LINE
00348      MOVE 'TOTAL RECORDS WRITTEN' TO T-MSG
00349      MOVE RECS-WRITTEN TO T-CNTR
00350      MOVE 'TOTAL IMPROVEMENT VALUATION' TO T-MSG-2
00351      MOVE TOTAL-IMPRV TO T-CNTR-2
00352      WRITE PRINT-REC FROM TOTAL-MSG-LINE AFTER ADVANCING 1
00353      MOVE SPACES TO TOTAL-MSG-LINE
00354      MOVE 'TOTAL RECORDS PRINTED' TO T-MSG
00355      MOVE RECS-PRINTED TO T-CNTR
00356      MOVE 'TOTAL VALUATION' TO T-MSG-2
00357      MOVE TOTAL-TOTAL TO T-CNTR-2
00358      WRITE PRINT-REC FROM TOTAL-MSG-LINE AFTER ADVANCING 1
00359      MOVE SPACES TO TOTAL-MSG-LINE-2
00360      MOVE 'TOTAL NON-RESIDENTIAL LAND VALUATION' TO T-MSG-2V
00361      MOVE TOTAL-TAX-LAND TO T-CNTR-2V
00362      WRITE PRINT-REC FROM TOTAL-MSG-LINE-2 AFTER ADVANCING 2
00363      MOVE 'TOTAL NON-RESIDENTIAL IMPROVEMENT VALUATION'
00364          TO T-MSG-2V
00365      MOVE TOTAL-TAX-IMPRV TO T-CNTR-2V
00366      WRITE PRINT-REC FROM TOTAL-MSG-LINE-2 AFTER ADVANCING 1
00367      MOVE 'TOTAL NON-RESIDENTIAL VALUATION' TO T-MSG-2V
00368      MOVE TOTAL-TAX-VALUE TO T-CNTR-2V
00369      WRITE PRINT-REC FROM TOTAL-MSG-LINE-2 AFTER ADVANCING 1
00370      MOVE SPACES TO TOTAL-MSG-LINE-2
00371      MOVE 'TOTAL RESIDENTIAL LAND VALUATION' TO T-MSG-2V
00372      MOVE TOTAL-NON-LAND TO T-CNTR-2V
00373      WRITE PRINT-REC FROM TOTAL-MSG-LINE-2 AFTER ADVANCING 2
00374      MOVE 'TOTAL RESIDENTIAL IMPROVEMENT VALUATION' TO T-MSG-2V
00375      MOVE TOTAL-NON-IMPRV TO T-CNTR-2V
00376      WRITE PRINT-REC FROM TOTAL-MSG-LINE-2 AFTER ADVANCING 1
00377      MOVE 'TOTAL RESIDENTIAL VALUATION' TO T-MSG-2V
00378      MOVE TOTAL-NON-VALUE TO T-CNTR-2V
00379      WRITE PRINT-REC FROM TOTAL-MSG-LINE-2 AFTER ADVANCING 1.
00380  900-HEADING-RTN.
00381      ADD +1 TO PAGE-CNTR
00382      MOVE PAGE-CNTR TO HD1-PAGE
00383      WRITE PRINT-REC FROM HEADING1 AFTER ADVANCING PAGE
00384      WRITE PRINT-REC FROM HEADING2 AFTER ADVANCING 2
00385      WRITE PRINT-REC FROM HEADING3 AFTER ADVANCING 2
00386      WRITE PRINT-REC FROM HEADING4 AFTER ADVANCING 1
00387      MOVE +6 TO LINE-CNTR.
