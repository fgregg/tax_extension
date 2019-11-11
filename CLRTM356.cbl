00001  IDENTIFICATION DIVISION.
00002  PROGRAM-ID. CLRTM356.
00003  AUTHOR. N.FLEMING-K.KISTINGER.
00004  DATE-WRITTEN. JULY 8, 1996.
00005 *REMARKS.
00006 *        THIS PROGRAM IS DESIGNED TO REMOVE THE TIF PARCELS FROM
00007 *        THE ASSESSMENT MASTER FILE, CREATING THE NEXT GENERATION
00008 *        OF THE ASSESSMENT FILE MINUS THESE PARCELS.
00009      SKIP1
00010 *****************************************************************
00011 *           PROGRAM WORK REQUEST (MODIFICATION)                 *
00012 *                                                               *
00013 * PROGRAMMER: DPSILVA                                           *
00014 * DATE: 08/20/99                                                *
00015 * REQUEST LETTER NAME: CL071799.M01                             *
00016 *****************************************************************
00017 *                MODIFICATION DESCRIPTION                       *
00018 *                                                               *
00019 * MODIFIED PROGRAM CLRTM356 FOR YEAR 2000.                      *
00020 *                                                               *
00021 * 1. IN WORKING STORAGE:                                        *
00022 *   A. EXPANDED PH2-YR FROM 2 TO 4, AND CHANGE THE PROGRAM TO   *
00023 *      DISPLAY THE CURRENT 4 DIGIT YEAR IN THIS FIELD. ALSO,    *
00024 *      REMOVE PH2-CC THAT PRECEDES IT.                          *
00025 * 2. IN THE LINKAGE SECTION, CHANGE LINK-TAX-YEAR TO A 4 POSI-  *
00026 *    TION NUMERIC FIELD AND REMOVE THE REDEFINITION THAT        *
00027 *    FOLLOWS IT.                                                *
00028 *****************************************************************
00029  ENVIRONMENT DIVISION.
00030  CONFIGURATION SECTION.
00031  SOURCE-COMPUTER. IBM-370.
00032  OBJECT-COMPUTER. IBM-370.
00033      SKIP1
00034  INPUT-OUTPUT SECTION.
00035      SKIP3
00036  FILE-CONTROL.
00037      SELECT CLTIFMST      ASSIGN TO UT-S-TIFDETL.
00038      SELECT MASTER-IN     ASSIGN TO UT-S-ASSESMST.
00039      SELECT MASTER-OUT    ASSIGN TO UT-S-ASSESOUT.
00040      SELECT REPORT-FILE   ASSIGN TO UT-S-REPORT.
00041      EJECT
00042  DATA DIVISION.
00043  FILE SECTION.
00044      SKIP3
00045  COPY CLTIFMSTF1.
00046      SKIP3
00047  COPY ASREASFD01.
00048      SKIP3
00049  COPY ASREASFD02.
00050      SKIP3
00051  FD  REPORT-FILE
00052      RECORDING MODE IS F
00053      BLOCK CONTAINS 0 RECORDS
00054      RECORD CONTAINS 133 CHARACTERS
00055      LABEL RECORDS ARE STANDARD
00056      DATA RECORD IS REPORT-REC.
00057      SKIP1
00058  01  REPORT-REC          PIC X(133).
00059      EJECT
00060  WORKING-STORAGE SECTION.
00061  77  TIFDETAIL-EOF-SW        PIC X   VALUE 'N'.
00062      88  TIFDETAIL-EOF               VALUE 'Y'.
00063      88  TIFDETAIL-NOT-EOF           VALUE 'N'.
00064      SKIP1
00065  77  MASTER-IN-EOF-SW        PIC X   VALUE 'N'.
00066      88  MASTER-IN-EOF               VALUE 'Y'.
00067      88  MASTER-IN-NOT-EOF           VALUE 'N'.
00068      SKIP1
00069  77  TIF-1ST-REC-SW          PIC X   VALUE 'Y'.
00070      88  TIF-1ST-REC                 VALUE 'Y'.
00071      88  TIF-REST-OF-RECS            VALUE 'N'.
00072      SKIP1
00073  77  M-1ST-REC-SW            PIC X   VALUE 'Y'.
00074      88  M-1ST-REC                   VALUE 'Y'.
00075      88  M-REST-OF-RECS              VALUE 'N'.
00076      SKIP1
00077  77  ERR-SW                  PIC X   VALUE 'N'.
00078      88  FILE-ERR                    VALUE 'Y'.
00079      88  FILE-OK                     VALUE 'N'.
00080      SKIP1
00081  77  BLANK-LINE              PIC X     VALUE SPACE.
00082  77  PAGE-CNTR               PIC S9(5) PACKED-DECIMAL VALUE +0.
00083  77  LINE-CNTR               PIC S9(3) PACKED-DECIMAL VALUE +70.
00084  77  REST-OF-LINE-CNTR       PIC S9(3) PACKED-DECIMAL VALUE +0.
00085  77  TIFDETAIL-RECS-READ     PIC S9(9) PACKED-DECIMAL VALUE +0.
00086  77  TIFDETAIL-RECS-MATCH    PIC S9(9) PACKED-DECIMAL VALUE +0.
00087  77  MASTER-IN-RECS-READ     PIC S9(9) PACKED-DECIMAL VALUE +0.
00088  77  MASTER-OUT-RECS-WRIT    PIC S9(9) PACKED-DECIMAL VALUE +0.
00089  77  REPORT-RECS-WRIT        PIC S9(9) PACKED-DECIMAL VALUE +0.
00090      SKIP1
00091  01  HOLD-AREA.
00092      SKIP1
00093      03  WORK-TAX-CODE          PIC 9(5).
00094      03  FILLER REDEFINES WORK-TAX-CODE.
00095          05  W-TOWN             PIC 9(2).
00096          05  FILLER             PIC 9(3).
00097      SKIP1
00098      03  TIFDETAIL-KEY          PIC X(19).
00099      03  FILLER REDEFINES TIFDETAIL-KEY.
00100          05  TIF-TOWN-KEY       PIC 9(2).
00101          05  TIF-VOLUME-KEY     PIC 9(3).
00102          05  TIF-PROP-KEY       PIC 9(14).
00103      SKIP1
00104      03  PREV-TIFDETAIL-KEY     PIC X(19).
00105      03  FILLER REDEFINES PREV-TIFDETAIL-KEY.
00106          05  P-TIF-TOWN-KEY     PIC 9(2).
00107          05  P-TIF-VOLUME-KEY   PIC 9(3).
00108          05  P-TIF-PROP-KEY     PIC 9(14).
00109      SKIP1
00110      03  MASTER-IN-KEY          PIC X(19).
00111      03  FILLER REDEFINES MASTER-IN-KEY.
00112          05  M-TOWN-KEY         PIC 9(2).
00113          05  M-VOLUME-KEY       PIC 9(3).
00114          05  M-PROP-KEY         PIC 9(14).
00115      SKIP1
00116      03  PREV-MASTER-IN-KEY     PIC X(19).
00117      03  FILLER REDEFINES PREV-MASTER-IN-KEY.
00118          05  P-M-TOWN-KEY       PIC 9(2).
00119          05  P-M-VOLUME-KEY     PIC 9(3).
00120          05  P-M-PROP-KEY       PIC 9(14).
00121      SKIP1
00122  01  WORK-AREA.
00123      SKIP1
00124      03  SUBS.
00125          05  SUB             PIC S9(4)  PACKED-DECIMAL VALUE +0.
00126          05  SUB1            PIC S9(4)  PACKED-DECIMAL VALUE +0.
00127      SKIP1
00128  01  PRINT-LINES.
00129      05  PAGE-HDR-1.
00130          10  FILLER          PIC X      VALUE SPACE.
00131          10  FILLER          PIC X(8)   VALUE 'CLRTM356'.
00132          10  FILLER          PIC X(45)  VALUE SPACES.
00133          10  FILLER          PIC X(26)  VALUE
00134          'OFFICE OF THE COUNTY CLERK'.
00135          10  FILLER          PIC X(39)  VALUE SPACES.
00136          10  FILLER          PIC X(9)   VALUE 'PAGE NO. '.
00137          10  PH1-PAGE-NO     PIC Z,ZZ9.
00138      SKIP1
00139      05  PAGE-HDR-2.
00140          10  FILLER          PIC X      VALUE SPACE.
00141          10  FILLER          PIC X(5)   VALUE 'DATE '.
00142          10  PH2-MO          PIC 99.
00143          10  FILLER          PIC X      VALUE '/'.
00144          10  PH2-DA          PIC 99.
00145          10  FILLER          PIC X      VALUE '/'.
00146          10  PH2-YR          PIC 9999.
00147          10  FILLER          PIC X(7)   VALUE SPACES.
00148          10  FILLER          PIC X(9)   VALUE 'TAX YEAR '.
00149          10  PH2-TAX-YEAR    PIC 9(4).
00150          10  FILLER          PIC X(7)   VALUE SPACES.
00151          10  FILLER          PIC X(49)  VALUE
00152          'TIF DETAIL PARCELS DELETED FROM ASSESSMENT MASTER'.
00153      SKIP1
00154      05  PAGE-HDR-3.
00155          10  FILLER          PIC X(2)   VALUE SPACES.
00156          10  FILLER          PIC X(6)   VALUE 'VOLUME'.
00157          10  FILLER          PIC X(2)   VALUE SPACES.
00158          10  FILLER          PIC X(21)  VALUE
00159          'PROPERTY INDEX NUMBER'.
00160          10  FILLER          PIC X(3)   VALUE SPACES.
00161          10  FILLER          PIC X(7)   VALUE 'TX-CODE'.
00162          10  FILLER          PIC X(3)   VALUE SPACES.
00163          10  FILLER          PIC X(10)  VALUE 'TXYR-ADDED'.
00164          10  FILLER          PIC X(3)   VALUE SPACES.
00165          10  FILLER          PIC X(22)  VALUE
00166          'FROZEN EQUALIZED VALUE'.
00167      SKIP1
00168      05  DETAIL-LINE.
00169          10  FILLER          PIC X(3)   VALUE SPACES.
00170          10  DTL-VOL         PIC 9(3).
00171          10  FILLER          PIC X(5)   VALUE SPACES.
00172          10  DTL-PROP-NO     PIC 99,99,999,999,9999.
00173          10  FILLER          PIC X(6)   VALUE SPACES.
00174          10  DTL-TAX-CODE    PIC 9(5).
00175          10  FILLER          PIC X(7)   VALUE SPACES.
00176          10  DTL-TAX-YEAR-ADDED  PIC 9999.
00177          10  FILLER          PIC X(9)   VALUE SPACES.
00178          10  DTL-FR-EQ-VALUE   PIC ZZ,ZZZ,ZZZ,ZZ9.
00179      SKIP1
00180      05  TOTAL-LINE.
00181          10  FILLER          PIC X(5)   VALUE SPACES.
00182          10  FILLER          PIC X(35)  VALUE
00183          'TOTAL NUMBER OF TIF DETAILS DELETED'.
00184          10  FILLER          PIC X(3)   VALUE SPACES.
00185          10  TL-NO-OF-TIF-DTLS-DELETED    PIC ZZZ,ZZ9.
00186      SKIP1
00187  01  WORK-AREAS.
00188      05  DSP-TIME            PIC 99,99.
00189      05  DSP-DATE.
00190          10  DSP-MO          PIC 99.
00191          10  FILLER          PIC X      VALUE '/'.
00192          10  DSP-DA          PIC 99.
00193          10  FILLER          PIC X      VALUE '/'.
00194          10  DSP-YR          PIC 9999.
00195      05  ACPT-DATE           PIC 9(8).
00196      05  ACPT-TIME-HOLD      PIC 9(8).
00197      05  ACPT-TIME-HOLD-X REDEFINES ACPT-TIME-HOLD.
00198          10  ACPT-TIME       PIC 9(4).
00199          10  FILLER          PIC 9(4).
00200      SKIP1
00201      EJECT
00202  LINKAGE SECTION.
00203      SKIP1
00204  01  LINK-INFO.
00205      05  LINK-LENGTH         PIC S9(4) COMP.
00206          88  VALID-LINK-LENGTH   VALUE +4.
00207      05  LINK-TAX-YEAR       PIC 9(4).
00208      SKIP1
00209  PROCEDURE DIVISION USING LINK-INFO.
00210
00211  0000-INITIALIZE.
00212      ACCEPT ACPT-TIME-HOLD FROM TIME.
00213      MOVE ACPT-TIME TO DSP-TIME.
00214      INSPECT DSP-TIME REPLACING ALL ',' BY ':'.
00215      MOVE FUNCTION CURRENT-DATE(1:8) TO ACPT-DATE.
00216      MOVE ACPT-DATE(5:2)    TO DSP-MO, PH2-MO.
00217      MOVE ACPT-DATE(7:2)    TO DSP-DA, PH2-DA.
00218      MOVE ACPT-DATE(1:4)    TO DSP-YR, PH2-YR.
00219      DISPLAY 'PROGRAM CLRTM356 DATE AND TIME OF RUN ' DSP-DATE
00220          ' ' DSP-TIME.
00221      DISPLAY SPACE.
00222      SKIP1
00223      IF NOT VALID-LINK-LENGTH
00224          DISPLAY 'LINKAGE LENGTH INVALID'
00225          MOVE 0016 TO RETURN-CODE
00226          STOP RUN.
00227      IF (LINK-TAX-YEAR NOT NUMERIC)
00228          OR (LINK-TAX-YEAR NOT GREATER THAN '0000')
00229              DISPLAY 'INVALID TAX YEAR PARAMETER  ' LINK-TAX-YEAR
00230              MOVE 0016 TO RETURN-CODE
00231              STOP RUN
00232      ELSE
00233          MOVE LINK-TAX-YEAR TO PH2-TAX-YEAR
00234      END-IF.
00235      OPEN  INPUT  CLTIFMST
00236                   MASTER-IN.
00237      OPEN  OUTPUT MASTER-OUT
00238                   REPORT-FILE.
00239      WRITE REPORT-REC FROM BLANK-LINE AFTER ADVANCING PAGE.
00240      MOVE ZEROES TO HOLD-AREA.
00241      PERFORM 0200-READ-TIFDETAIL.
00242      PERFORM 0300-READ-MASTER-IN.
00243      PERFORM 0100-MAINLINE
00244          UNTIL (TIFDETAIL-EOF AND MASTER-IN-EOF) OR FILE-ERR.
00245      IF LINE-CNTR GREATER THAN 57
00246          PERFORM 1000-HEADING-ROUTINE
00247      ELSE
00248          SUBTRACT LINE-CNTR FROM 57 GIVING REST-OF-LINE-CNTR
00249          PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB EQUAL
00250              REST-OF-LINE-CNTR
00251                  WRITE REPORT-REC FROM BLANK-LINE AFTER
00252                      ADVANCING 1
00253          END-PERFORM
00254      END-IF.
00255      MOVE TIFDETAIL-RECS-MATCH TO TL-NO-OF-TIF-DTLS-DELETED.
00256      WRITE REPORT-REC FROM TOTAL-LINE AFTER ADVANCING 2.
00257      DISPLAY 'TIF DETAIL RECORDS READ     ' TIFDETAIL-RECS-READ.
00258      DISPLAY 'MASTER IN RECORDS READ      ' MASTER-IN-RECS-READ.
00259      DISPLAY 'MASTER OUT RECORDS WRITTEN  ' MASTER-OUT-RECS-WRIT.
00260      DISPLAY 'TIF DETAIL RECORDS MATCHED  ' TIFDETAIL-RECS-MATCH.
00261      DISPLAY 'REPORT RECORDS WRITTEN      ' REPORT-RECS-WRIT.
00262      CLOSE CLTIFMST
00263            MASTER-IN
00264            MASTER-OUT
00265            REPORT-FILE.
00266      STOP RUN.
00267      SKIP1
00268  0100-MAINLINE.
00269      IF TIFDETAIL-KEY EQUAL MASTER-IN-KEY
00270          PERFORM 0500-PRINT-REPORT
00271          ADD +1 TO TIFDETAIL-RECS-MATCH
00272          PERFORM 0200-READ-TIFDETAIL
00273          PERFORM 0300-READ-MASTER-IN
00274      ELSE
00275          IF MASTER-IN-KEY GREATER THAN TIFDETAIL-KEY
00276              PERFORM 0200-READ-TIFDETAIL
00277          ELSE
00278              IF MASTER-IN-KEY LESS THAN TIFDETAIL-KEY
00279                  PERFORM 0400-WRITE-MASTER-OUT
00280              END-IF
00281          END-IF
00282      END-IF.
00283      SKIP1
00284  0200-READ-TIFDETAIL.
00285      READ  CLTIFMST  AT END
00286          MOVE ALL '9' TO TIFDETAIL-KEY
00287          MOVE 'Y' TO TIFDETAIL-EOF-SW.
00288      IF NOT TIFDETAIL-EOF
00289          ADD +1 TO TIFDETAIL-RECS-READ
00290          IF TIF-1ST-REC
00291              MOVE TF-PROP TO TIF-PROP-KEY
00292              MOVE TF-TXCD TO WORK-TAX-CODE
00293              MOVE W-TOWN  TO TIF-TOWN-KEY
00294              MOVE TF-VOL TO TIF-VOLUME-KEY
00295              MOVE 'N' TO TIF-1ST-REC-SW
00296          ELSE
00297              MOVE TIFDETAIL-KEY TO PREV-TIFDETAIL-KEY
00298              MOVE TF-PROP TO TIF-PROP-KEY
00299              MOVE TF-TXCD TO WORK-TAX-CODE
00300              MOVE W-TOWN TO TIF-TOWN-KEY
00301              MOVE TF-VOL TO TIF-VOLUME-KEY
00302          END-IF
00303      END-IF.
00304      IF TIFDETAIL-KEY LESS THAN PREV-TIFDETAIL-KEY
00305          MOVE 'Y' TO ERR-SW
00306          DISPLAY 'CLTIFMSTR1 OUT OF SEQUENCE'
00307          DISPLAY 'CURRENT TIFDETAIL KEY IS  '
00308              TIF-TOWN-KEY, TIF-VOLUME-KEY, TIF-PROP-KEY
00309          DISPLAY 'PREVIOUS TIFDETAIL KEY IS '
00310              P-TIF-TOWN-KEY, P-TIF-VOLUME-KEY, P-TIF-PROP-KEY
00311          MOVE 0016 TO RETURN-CODE
00312      END-IF.
00313      SKIP1
00314  0300-READ-MASTER-IN.
00315      READ MASTER-IN AT END
00316          MOVE ALL '9' TO MASTER-IN-KEY
00317          MOVE 'Y' TO MASTER-IN-EOF-SW.
00318      IF NOT MASTER-IN-EOF
00319          ADD +1 TO MASTER-IN-RECS-READ
00320          IF M-1ST-REC
00321              MOVE M-PROP OF MSTR-IN-REC TO M-PROP-KEY
00322              MOVE M-TXCD OF MSTR-IN-REC TO WORK-TAX-CODE
00323              MOVE W-TOWN TO M-TOWN-KEY
00324              MOVE M-VOL OF MSTR-IN-REC TO M-VOLUME-KEY
00325              MOVE 'N' TO M-1ST-REC-SW
00326          ELSE
00327              MOVE MASTER-IN-KEY TO PREV-MASTER-IN-KEY
00328              MOVE M-PROP OF MSTR-IN-REC TO M-PROP-KEY
00329              MOVE M-TXCD OF MSTR-IN-REC TO WORK-TAX-CODE
00330              MOVE W-TOWN TO M-TOWN-KEY
00331              MOVE M-VOL OF MSTR-IN-REC TO M-VOLUME-KEY
00332          END-IF
00333      END-IF.
00334      IF MASTER-IN-KEY LESS THAN PREV-MASTER-IN-KEY
00335          MOVE 'Y' TO ERR-SW
00336          DISPLAY 'ASSESSMENT MASTER FILE OUT OF SEQUENCE'
00337          DISPLAY 'CURRENT MASTER-IN KEY IS  '
00338              M-TOWN-KEY, M-PROP-KEY, M-VOLUME-KEY
00339          DISPLAY 'PREVIOUS MASTER-IN KEY IS '
00340              P-M-TOWN-KEY, P-M-PROP-KEY, P-M-VOLUME-KEY
00341          MOVE 0016 TO RETURN-CODE
00342      END-IF.
00343      SKIP1
00344  0400-WRITE-MASTER-OUT.
00345      WRITE MSTR-OUT-REC FROM MSTR-IN-REC.
00346      ADD +1 TO MASTER-OUT-RECS-WRIT.
00347      PERFORM 0300-READ-MASTER-IN.
00348      SKIP1
00349  0500-PRINT-REPORT.
00350      IF LINE-CNTR GREATER THAN 59
00351          PERFORM 1000-HEADING-ROUTINE.
00352      MOVE TF-VOL TO DTL-VOL.
00353      MOVE TF-PROP  TO DTL-PROP-NO.
00354      INSPECT DTL-PROP-NO REPLACING ALL ',' BY '-'.
00355      MOVE TF-TXCD TO DTL-TAX-CODE.
00356      MOVE TF-TXYR TO DTL-TAX-YEAR-ADDED.
00357      MOVE TF-FRZEQVAL TO DTL-FR-EQ-VALUE.
00358      WRITE REPORT-REC FROM DETAIL-LINE AFTER ADVANCING 2.
00359      ADD +2 TO LINE-CNTR.
00360      ADD +1 TO REPORT-RECS-WRIT.
00361      SKIP1
00362  1000-HEADING-ROUTINE.
00363      ADD +1 TO PAGE-CNTR.
00364      MOVE PAGE-CNTR TO PH1-PAGE-NO.
00365      WRITE REPORT-REC FROM PAGE-HDR-1 AFTER ADVANCING PAGE.
00366      WRITE REPORT-REC FROM PAGE-HDR-2 AFTER ADVANCING 2.
00367      WRITE REPORT-REC FROM PAGE-HDR-3 AFTER ADVANCING 3.
00368      MOVE +6 TO LINE-CNTR.
00369      SKIP1