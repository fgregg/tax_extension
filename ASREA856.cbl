00001  IDENTIFICATION DIVISION.                                         11/23/92
00002  PROGRAM-ID. ASREA856.                                            ASREA856
00003  AUTHOR. LIAUBA - MOLIS.                                             LV003
00004  DATE-WRITTEN. NOVEMBER 20,1992.                                  ASREA856
00005 *REMARKS. THE PURPOSE OF THIS PROGRAM IS TO CREATE THE            ASREA856
00006 *         TOWNSHIP HOMEOWNER FILES.                               ASREA856
00007      SKIP3                                                        ASREA856
00008  ENVIRONMENT DIVISION.                                            ASREA856
00009  CONFIGURATION SECTION.                                           ASREA856
00010  SOURCE-COMPUTER. IBM-370.                                        ASREA856
00011  OBJECT-COMPUTER. IBM-370.                                        ASREA856
00012      SKIP1                                                        ASREA856
00013  INPUT-OUTPUT SECTION.                                            ASREA856
00014  FILE-CONTROL.                                                    ASREA856
00015      SELECT HOMEOWNER-MAST ASSIGN TO UT-S-MAST.                   ASREA856
00016      SELECT TEMPOUT ASSIGN TO UT-S-TEMPOUT.                       ASREA856
00017  DATA DIVISION.                                                   ASREA856
00018  FILE SECTION.                                                    ASREA856
00019      SKIP1                                                        ASREA856
00020  FD  HOMEOWNER-MAST                                               ASREA856
00021      BLOCK CONTAINS 0 RECORDS                                     ASREA856
00022      RECORD CONTAINS 130 CHARACTERS                               ASREA856
00023      LABEL RECORDS ARE STANDARD                                   ASREA856
00024      DATA RECORD IS HO-REC.                                       ASREA856
00025  01  HO-REC.                                                      ASREA856
00026  COPY HOMEXERD01.                                                 ASREA856 FOUND   LEVEL=016 DATE=07/14/08.
00027      SKIP2                                                        ASREA856
00028  FD  TEMPOUT                                                      ASREA856
00029      BLOCK CONTAINS 0 RECORDS                                     ASREA856
00030      RECORD CONTAINS 130 CHARACTERS                               ASREA856
00031      LABEL RECORDS ARE STANDARD                                   ASREA856
00032      DATA RECORD IS TEMPOUT-REC.                                  ASREA856
00033  01  TEMPOUT-REC       PIC X(130).                                ASREA856
00034      SKIP2                                                        ASREA856
00035  WORKING-STORAGE SECTION.                                         ASREA856
00036      SKIP1                                                        ASREA856
00037  77  FILLER                  PIC X(58)             VALUE          ASREA856
00038      '***PROGRAM ASREA856 WORKING-STORAGE SECTION STARTS HERE***'.ASREA856
00039  77  EOF-SW                  PIC X                 VALUE 'N'.     ASREA856
00040      88  EOF                                       VALUE 'Y'.     ASREA856
00041  77  HOMEOWNER-RECS-RD       PIC S9(7)    PACKED-DECIMAL VALUE +0.ASREA856
00042  77  TEMPOUT-RECS-WRITTEN    PIC S9(7)    PACKED-DECIMAL VALUE +0.ASREA856
00043  77  RECS-RD-CNT             PIC S9(7)    PACKED-DECIMAL VALUE +0.ASREA856
00044      88  FIRST-REC                                 VALUE +1.      ASREA856
00045  77  BLNK                    PIC X       VALUE SPACE.             ASREA856
00046  77  RUN-DATE                PIC X(8).                            ASREA856
00047      SKIP1                                                        ASREA856
00048  77  ERR-SW                  PIC X       VALUE 'N'.               ASREA856
00049      88  ERR                 VALUE 'Y'.                           ASREA856
00050      SKIP1                                                        ASREA856
00051  01  WORK-AREA.                                                   ASREA856
00052      05  CURR-KEY.                                                ASREA856
00053          10  CURR-TOWN           PIC 9(2).                        ASREA856
00054          10  CURR-VOL            PIC 9(3).                        ASREA856
00055          10  CURR-PROP           PIC 9(15).                       ASREA856
00056          10  CURR-TXTYP          PIC 9.                           ASREA856
00057      05  SAVE-KEY.                                                ASREA856
00058          10  SAVE-TOWN           PIC 9(2)   VALUE ZEROS.          ASREA856
00059      SKIP1                                                        ASREA856
00060      05  WS-TWN.                                                  ASREA856
00061          10  WS-TOWN             PIC 99.                          ASREA856
00062             88  SV-SUBURBAN                    VALUE 10 THRU 39.  ASREA856
00063             88  SV-CITY                        VALUE 70 THRU 77.  ASREA856
00064      05  WS-TOWN-N REDEFINES WS-TWN  PIC 9(2).                    ASREA856
00065      05  PREV-KEY                PIC X(21) VALUE LOW-VALUES.      ASREA856
00066      05  W-UNPK-TXCDE  PIC 9(5).                                  ASREA856
00067      05  FILLER REDEFINES W-UNPK-TXCDE.                           ASREA856
00068          10 W-TOWN     PIC 9(2).                                  ASREA856
00069          10 W-REST     PIC 9(3).                                  ASREA856
00070      05  DSP-TIME                   PIC 99,99.                    ASREA856
00071      05  DSP-DATE.                                                ASREA856
00072          10  DSP-MO                 PIC 99.                       ASREA856
00073          10  FILLER                 PIC X         VALUE '/'.      ASREA856
00074          10  DSP-DA                 PIC 99.                       ASREA856
00075          10  FILLER                 PIC X         VALUE '/'.      ASREA856
00076          10  DSP-YR                 PIC 99.                       ASREA856
00077      05  ACPT-DATE                  PIC 9(6).                     ASREA856
00078      05  ACPT-DATE-X REDEFINES ACPT-DATE.                         ASREA856
00079          10  ACPT-YR                PIC 99.                       ASREA856
00080          10  ACPT-MO                PIC 99.                       ASREA856
00081          10  ACPT-DA                PIC 99.                       ASREA856
00082      05  ACPT-TIME-HOLD             PIC 9(8).                     ASREA856
00083      05  ACPT-TIME-HOLD-X REDEFINES ACPT-TIME-HOLD.               ASREA856
00084          10  ACPT-TIME              PIC 9(4).                     ASREA856
00085          10  FILLER                 PIC 9(4).                     ASREA856
00086      05  WS-TIME                    PIC 99,99,99.                 ASREA856
00087  COPY TWNINFOTBL.                                                 ASREA856 FOUND   LEVEL=010 DATE=10/03/19.
00088      EJECT                                                        ASREA856
00089  LINKAGE SECTION.                                                 ASREA856
00090  01  LINK-INFO.                                                   ASREA856
00091      05  LINK-LENGTH      PIC S9(4)   COMP.                       ASREA856
00092          88  VALID-LENGTH VALUE +2.                               ASREA856
00093      SKIP1                                                        ASREA856
00094      05  LINK-TOWN.                                               ASREA856
00095          10  LINK-TOWN-N   PIC 99.                                ASREA856
00096              88  VAL-L-TWN    VALUE 10 THRU 39  70 THRU 77.       ASREA856
00097      EJECT                                                        ASREA856
00098  PROCEDURE DIVISION USING LINK-INFO.                              ASREA856
00099      SKIP1                                                        ASREA856
00100  000-HOUSEKEEPING.                                                ASREA856
00101  SKIP2                                                            ASREA856
00102      IF NOT VALID-LENGTH                                          ASREA856
00103          DISPLAY 'LINKAGE LENGTH INVALID OR MISSING'              ASREA856
00104          MOVE 16 TO RETURN-CODE                                   ASREA856
00105          STOP RUN.                                                ASREA856
00106      SKIP1                                                        ASREA856
00107      IF LINK-TOWN NOT NUMERIC                                     ASREA856
00108       OR                                                          ASREA856
00109         NOT VAL-L-TWN                                             ASREA856
00110          DISPLAY 'TOWN LINKAGE MUST BE EQUAL TO 10 THRU 39 '      ASREA856
00111          DISPLAY 'OR 70 THRU 77'                                  ASREA856
00112          MOVE 16 TO RETURN-CODE.                                  ASREA856
00113      IF RETURN-CODE EQUAL 16                                      ASREA856
00114          STOP RUN.                                                ASREA856
00115      ACCEPT ACPT-TIME-HOLD FROM TIME.                             ASREA856
00116      MOVE ACPT-TIME TO DSP-TIME.                                  ASREA856
00117      INSPECT DSP-TIME REPLACING ALL ',' BY ':'.                   ASREA856
00118      ACCEPT ACPT-DATE FROM DATE.                                  ASREA856
00119      MOVE ACPT-MO TO DSP-MO.                                      ASREA856
00120      MOVE ACPT-DA TO DSP-DA.                                      ASREA856
00121      MOVE ACPT-YR TO DSP-YR.                                      ASREA856
00122      DISPLAY 'PROGRAM ASREA856 DATE AND TIME OF RUN = '           ASREA856
00123        DSP-DATE ' ' DSP-TIME.                                     ASREA856
00124      MOVE DSP-DATE TO RUN-DATE                                       CL**2
00125      MOVE  DSP-TIME      TO WS-TIME                               ASREA856
00126      INSPECT WS-TIME REPLACING ALL ',' BY '-'                     ASREA856
00127      DISPLAY 'WS-TIME ' WS-TIME                                   ASREA856
00128      OPEN INPUT HOMEOWNER-MAST                                    ASREA856
00129           OUTPUT TEMPOUT                                          ASREA856
00130      SKIP1                                                        ASREA856
00131      PERFORM 020-MAINLINE UNTIL ERR OR EOF                        ASREA856
00132              OR SAVE-TOWN  GREATER THAN                           ASREA856
00133                 LINK-TOWN-N                                       ASREA856
00134      IF  TEMPOUT-RECS-WRITTEN EQUAL ZERO                          ASREA856
00135          MOVE 16 TO RETURN-CODE.                                  ASREA856
00136      SKIP1                                                        ASREA856
00137      DISPLAY 'ASREA856 SUMMARY FOR ' RUN-DATE ' RUN'              ASREA856
00138      DISPLAY  SPACE                                               ASREA856
00139      DISPLAY 'TOTAL HOMEOWNER RECORDS READ '                      ASREA856
00140                         RECS-RD-CNT.                              ASREA856
00141      DISPLAY 'TOTAL HOMEOWNER RECORDS WRITTEN FOR TOWN '             CL**3
00142             LINK-TOWN-N SPACE TEMPOUT-RECS-WRITTEN.                  CL**3
00143      CLOSE  HOMEOWNER-MAST TEMPOUT                                ASREA856
00144      STOP RUN.                                                    ASREA856
00145      EJECT                                                        ASREA856
00146  020-MAINLINE.                                                    ASREA856
00147      PERFORM 030-READ-HOMEOWNER-FILE                              ASREA856
00148      IF NOT EOF AND NOT ERR                                       ASREA856
00149          AND SAVE-TOWN = LINK-TOWN-N                              ASREA856
00150          PERFORM 060-WRITE-TEMPOUT-RECORD.                        ASREA856
00151      SKIP3                                                        ASREA856
00152  030-READ-HOMEOWNER-FILE.                                         ASREA856
00153      READ HOMEOWNER-MAST     AT END                               ASREA856
00154           MOVE 'Y' TO EOF-SW.                                     ASREA856
00155      IF NOT EOF                                                   ASREA856
00156          MOVE  HM-TXCD         TO  W-UNPK-TXCDE                   ASREA856
00157          MOVE  W-TOWN          TO  CURR-TOWN                      ASREA856
00158          MOVE  CURR-TOWN       TO  SAVE-TOWN                      ASREA856
00159          MOVE  HM-VOL          TO  CURR-VOL                       ASREA856
00160          MOVE  HM-PROP         TO  CURR-PROP                      ASREA856
00161          MOVE  HM-TXTYP        TO  CURR-TXTYP                     ASREA856
00162      SKIP2                                                        ASREA856
00163          IF CURR-KEY LESS THAN  PREV-KEY                          ASREA856
00164              DISPLAY 'SEQUENCE ERROR IN HOMEOWNER EXEMPTION FILE 'ASREA856
00165              DISPLAY 'CURRENT-KEY =   ' SPACE CURR-TOWN           ASREA856
00166                                         SPACE CURR-VOL            ASREA856
00167                                         SPACE CURR-PROP           ASREA856
00168                                         SPACE CURR-TXTYP          ASREA856
00169              DISPLAY 'PREVIOUS-KEY =  ' SPACE PREV-KEY            ASREA856
00170              MOVE 16 TO RETURN-CODE                               ASREA856
00171              MOVE 'Y' TO ERR-SW                                   ASREA856
00172          ELSE                                                     ASREA856
00173              MOVE CURR-KEY TO PREV-KEY                            ASREA856
00174              ADD +1  TO RECS-RD-CNT.                              ASREA856
00175      SKIP3                                                        ASREA856
00176  060-WRITE-TEMPOUT-RECORD.                                        ASREA856
00177      WRITE TEMPOUT-REC FROM HO-REC                                ASREA856
00178      ADD  +1   TO    TEMPOUT-RECS-WRITTEN.                        ASREA856
00179  SKIP2                                                            ASREA856
