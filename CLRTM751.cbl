00001  IDENTIFICATION DIVISION.
00002  PROGRAM-ID. CLRTM751.
00003 *****************************************************************
00004 * AUTHOR                                                        *
00005 *       ANALYST    - R. KALEMBA                                 *
00006 *       PROGRAMMER - T. BROWN                                   *
00007 * DATE-WRITTEN                                                  *
00008 *       SEPTEMBER 8, 1995                                       *
00009 * REMARKS.                                                      *
00010 *       THIS PROGRAM IS DESIGNED TO APPLY THE DIVISION NUMBER   *
00011 *       TO THE REAL ESTATE DIVISION VALUVATION FILE RECORD. IT  *
00012 *       IS A COPY OF THE ASREA976 PROGRAM.                      *
00013 *****************************************************************
00014 *
00015 *****************************************************************
00016 *           PROGRAM WORK REQUEST (MODIFICATION)                 *
00017 *                                                               *
00018 * PROGRAMMER: DPSILVA                                           *
00019 * DATE: 09/02/99                                                *
00020 * REQUEST LETTER NAME: CL071799.001                             *
00021 *****************************************************************
00022 *                MODIFICATION DESCRIPTION                       *
00023 *                                                               *
00024 * MODIFIED PROGRAM CLRTM751 FOR YEAR 2000.                      *
00025 *                                                               *
00026 * 1. IN WORKING STORAGE:                                        *
00027 *   A). CHANGE THE PROGRAM TO DISPLAY THE CURRENT DATE ALONG    *
00028 *       WITH CENTURY WHEN THE DATE AND TIME ARE DISPLAYED.      *
00029 *****************************************************************
00030  ENVIRONMENT DIVISION.
00031  INPUT-OUTPUT SECTION.
00032  FILE-CONTROL.
00033      SELECT EQUAL-VALUE ASSIGN UT-S-EQUALVAL.
00034      SELECT OUTPUT-FILE ASSIGN UT-S-OUTPUT.
00035      SELECT REDIVN-MAST ASSIGN UT-S-DIVSION.
00036  EJECT
00037  DATA DIVISION.
00038  FILE SECTION.
00039  FD  EQUAL-VALUE
00040      BLOCK CONTAINS 0 CHARACTERS
00041      RECORD CONTAINS 115 CHARACTERS.
00042  01  VALUE-IN-REC.
00043  COPY EQVALRD01.
00044  EJECT
00045  FD  OUTPUT-FILE
00046      BLOCK CONTAINS 0 CHARACTERS
00047      RECORD CONTAINS 115 CHARACTERS.
00048  01  OUTPUT-REC.
00049  COPY CLEQVLDV01.
00050  EJECT
00051  COPY REDIVNFD01.
00052  EJECT
00053  WORKING-STORAGE SECTION.
00054  77  EOF-SW                PIC X              VALUE 'N'.
00055      88 EOF-MAST                              VALUE 'Y'.
00056  77  EOF-DSW               PIC X              VALUE 'N'.
00057      88 EOF-DIV                               VALUE 'Y'.
00058  77  TOT-MAST-RECS-WRITTEN PIC S9(7)  COMP-3  VALUE ZERO.
00059  77  TOT-MAST-RECS-READ    PIC S9(7)  COMP-3  VALUE ZERO.
00060  77  TOT-MAST-RECS-UPDT    PIC S9(7)  COMP-3  VALUE ZERO.
00061  77  TOT-DIVN-RECS-READ    PIC S9(7)  COMP-3  VALUE ZERO.
00062  77  DIVN-RECS-UNMATCH     PIC S9(7)  COMP-3  VALUE ZERO.
00063  01  WORK-AREAS.
00064      05  DSP-TIME                   PIC X(5).
00065      05  DSP-DATE                   PIC X(10).
00066      05  ACPT-DATE                  PIC 9(8).
00067      05  ACPT-TIME                  PIC 9(8).
00068      05  PREV-KEY-VAL.
00069          10  PREV-VOL-VAL           PIC 999.
00070          10  PREV-PROP-VAL          PIC 9(15).
00071      05  CURR-KEY-VAL.
00072          10  CURR-VOL-VAL           PIC 999.
00073          10  CURR-PROP-VAL          PIC 9(15).
00074      05  PREV-KEY-DET.
00075          10  PREV-VOL-DET           PIC 999.
00076          10  PREV-PROP-DET          PIC 9(15).
00077      05  CURR-KEY-DET.
00078          10  CURR-VOL-DET           PIC 999.
00079          10  CURR-PROP-DET          PIC 9(15).
00080  EJECT
00081  PROCEDURE DIVISION.
00082  000-HOUSEKEEPING.
00083      PERFORM 100-INITIAL.
00084      PERFORM 200-READ-VALUE.
00085      PERFORM 300-READ-DIVISION.
00086      PERFORM 400-MAINLINE UNTIL (EOF-MAST AND EOF-DIV)
00087              OR RETURN-CODE EQUAL 16.
00088      PERFORM 900-FINAL.
00089      STOP RUN.
00090  SKIP2
00091  100-INITIAL.
00092      OPEN INPUT EQUAL-VALUE REDIVN-MAST
00093          OUTPUT OUTPUT-FILE.
00094      ACCEPT ACPT-TIME FROM TIME.
00095      STRING ACPT-TIME(1:2) ':' ACPT-TIME(3:2)
00096             DELIMITED BY SIZE INTO DSP-TIME.
00097      ACCEPT ACPT-DATE FROM DATE.
00098      MOVE FUNCTION CURRENT-DATE(1:8) TO ACPT-DATE.
00099      STRING ACPT-DATE(5:2) '/' ACPT-DATE(7:2) '/'
00100             ACPT-DATE(1:4) DELIMITED BY SIZE INTO DSP-DATE.
00101      DISPLAY 'PROGRAM CLRTM751 DATE AND TIME OF RUN = '
00102               DSP-DATE ' ' DSP-TIME.
00103      MOVE ZERO TO PREV-KEY-VAL CURR-KEY-VAL
00104                   PREV-KEY-DET CURR-KEY-DET.
00105  EJECT
00106  200-READ-VALUE.
00107      READ EQUAL-VALUE AT END
00108           MOVE 'Y' TO EOF-SW
00109           MOVE HIGH-VALUES TO CURR-KEY-VAL.
00110      IF NOT EOF-MAST
00111         ADD +1 TO TOT-MAST-RECS-READ
00112         MOVE EQ-PROP TO CURR-PROP-VAL
00113         MOVE EQ-VOL  TO CURR-VOL-VAL
00114         IF CURR-KEY-VAL >= PREV-KEY-VAL
00115            MOVE CURR-KEY-VAL TO PREV-KEY-VAL
00116         ELSE
00117            DISPLAY 'EQUALIZED VALUATION FILE OUT OF SEQUENCE'
00118            DISPLAY 'PREVIOUS KEY  ' PREV-KEY-VAL
00119            DISPLAY 'CURRENT KEY   ' CURR-KEY-VAL
00120            MOVE +16 TO RETURN-CODE.
00121  SKIP2
00122  300-READ-DIVISION.
00123      READ REDIVN-MAST AT END
00124           MOVE 'Y' TO EOF-DSW
00125           MOVE HIGH-VALUES TO CURR-KEY-DET.
00126      IF NOT EOF-DIV
00127         ADD +1 TO TOT-DIVN-RECS-READ
00128         MOVE DIN-PROP  TO CURR-PROP-DET
00129         MOVE DIN-VOL   TO CURR-VOL-DET
00130         IF CURR-KEY-DET IS NOT GREATER THAN PREV-KEY-DET
00131            DISPLAY 'DIVISION RECORDS  OUT OF SEQUENCE '
00132            DISPLAY 'PREVIOUS KEY  ' PREV-KEY-DET
00133            DISPLAY 'CURRENT KEY   ' CURR-KEY-DET
00134            MOVE +16 TO RETURN-CODE
00135         ELSE
00136            MOVE CURR-KEY-DET TO PREV-KEY-DET.
00137  EJECT
00138  400-MAINLINE.
00139      IF CURR-KEY-DET IS LESS THAN CURR-KEY-VAL
00140         PERFORM 300-READ-DIVISION
00141      ELSE
00142         IF CURR-KEY-VAL IS EQUAL TO CURR-KEY-DET
00143            MOVE VALUE-IN-REC TO OUTPUT-REC
00144            MOVE DIN-DIV TO DV-DIV-PROP-NO
00145            ADD +1 TO TOT-MAST-RECS-UPDT
00146            PERFORM 500-WRITE-MASTER
00147            PERFORM 200-READ-VALUE
00148            PERFORM 300-READ-DIVISION
00149         ELSE
00150            MOVE VALUE-IN-REC TO OUTPUT-REC
00151            MOVE EQ-PROP TO DV-DIV-PROP-NO
00152            PERFORM 500-WRITE-MASTER
00153            PERFORM 200-READ-VALUE
00154         END-IF
00155      END-IF.
00156  SKIP2
00157  500-WRITE-MASTER.
00158      WRITE OUTPUT-REC.
00159      ADD +1 TO TOT-MAST-RECS-WRITTEN.
00160  SKIP2
00161  900-FINAL.
00162      DISPLAY 'TOTAL MASTER RECORDS READ         '
00163              TOT-MAST-RECS-READ.
00164      DISPLAY 'TOTAL DIVISION RECORDS READ       '
00165              TOT-DIVN-RECS-READ.
00166      DISPLAY 'TOTAL MASTER RECORDS WRITTEN      '
00167              TOT-MAST-RECS-WRITTEN.
00168      DISPLAY 'TOTAL MASTER RECORDS UPTD/DIVN NO '
00169              TOT-MAST-RECS-UPDT.
00170      DISPLAY 'TOTAL DIVISION RECORDS UNMATCHED  '
00171              DIVN-RECS-UNMATCH.
00172      CLOSE EQUAL-VALUE OUTPUT-FILE REDIVN-MAST.
00173  EJECT
