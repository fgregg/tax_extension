00001  ID DIVISION.
00002  PROGRAM-ID. ASREA005.
00003  AUTHOR. TOMME - MOLIS.
00004  DATE-WRITTEN. APRIL 1, 1985.
00005 *REMARKS. THIS PROGRAM DELETES PRINT ALIGNMENT RECORDS FROM PRINT
00006 *             TAPES.
00007      SKIP3
00008  ENVIRONMENT DIVISION.
00009  CONFIGURATION SECTION.
00010  SOURCE-COMPUTER. IBM-370.
00011  OBJECT-COMPUTER. IBM-370.
00012  INPUT-OUTPUT SECTION.
00013  FILE-CONTROL.
00014      SELECT IN-FILE  ASSIGN TO UT-S-INFILE.
00015      SELECT OUT-FILE ASSIGN TO UT-S-OUTFILE.
00016      SKIP3
00017  DATA DIVISION.
00018  FILE SECTION.
00019  FD  IN-FILE
00020      LABEL RECORDS ARE STANDARD
00021      RECORDING MODE IS F
00022      BLOCK CONTAINS 0 RECORDS
00023      RECORD CONTAINS 133 CHARACTERS
00024      DATA RECORD IS IN-REC.
00025  01  IN-REC                  PIC X(133).
00026      SKIP1
00027  FD  OUT-FILE
00028      LABEL RECORDS ARE STANDARD
00029      RECORDING MODE IS F
00030      BLOCK CONTAINS 0 RECORDS
00031      RECORD CONTAINS 133 CHARACTERS
00032      DATA RECORD IS OUT-REC.
00033  01  OUT-REC                 PIC X(133).
00034      SKIP1
00035  WORKING-STORAGE SECTION.
00036      SKIP1
00037  77  EOF-SW                  PIC X      VALUE 'N'.
00038      88 EOF                             VALUE 'Y'.
00039      SKIP1
00040  77  DELETE-VAL              PIC S9(7)  VALUE +0   COMP-3.
00041      SKIP1
00042  77  IN-REC-COUNT            PIC S9(7)  VALUE +0   COMP-3.
00043      SKIP1
00044  77  OUT-REC-COUNT           PIC S9(7)  VALUE +0   COMP-3.
00045      SKIP3
00046  LINKAGE SECTION.
00047  01  LINK-INFO.
00048      05  LK-LENGTH           PIC S9(4)               COMP.
00049          88 VALID-LENGTH                 VALUE +2.
00050      05  DELREC              PIC 9(2).
00051          88  VALID-NUMS                  VALUE 00 THRU 99.
00052      EJECT
00053  PROCEDURE DIVISION USING LINK-INFO.
00054  100-INITIAL.
00055      IF NOT VALID-LENGTH
00056          DISPLAY SPACES
00057          DISPLAY 'INCORRECT PARM LENGTH ' LK-LENGTH
00058          MOVE 16 TO RETURN-CODE
00059          STOP RUN.
00060      SKIP1
00061      IF NOT VALID-NUMS
00062          DISPLAY SPACES
00063          DISPLAY 'DELETE RECORD LINKAGE VALUE MUST BE EQUAL TO 00
00064 -          'TO 99'
00065          DISPLAY 'THE INCORRECT PARM IS ' DELREC
00066          MOVE 16 TO RETURN-CODE
00067          STOP RUN.
00068      SKIP1
00069      OPEN INPUT  IN-FILE
00070           OUTPUT OUT-FILE.
00071      SKIP1
00072      PERFORM 200-MAINLINE
00073        UNTIL EOF.
00074      SKIP1
00075      DISPLAY SPACES.
00076      DISPLAY 'TOTAL RECORDS READ    ' IN-REC-COUNT.
00077      SKIP1
00078      DISPLAY SPACES.
00079      DISPLAY 'TOTAL RECORDS WRITTEN ' OUT-REC-COUNT.
00080      SKIP1
00081      DISPLAY SPACES.
00082      COMPUTE DELETE-VAL = IN-REC-COUNT - OUT-REC-COUNT.
00083      DISPLAY 'TOTAL RECORDS DELETED ' DELETE-VAL.
00084      SKIP1
00085      CLOSE IN-FILE
00086            OUT-FILE.
00087      SKIP1
00088      STOP RUN.
00089      SKIP3
00090  200-MAINLINE.
00091      PERFORM 300-READ-IN-FILE.
00092      SKIP1
00093      IF NOT EOF
00094          IF DELREC IS EQUAL TO +00
00095              PERFORM 400-WRITE-REC
00096          ELSE
00097              ADD +1 TO DELETE-VAL
00098              IF DELETE-VAL IS GREATER THAN DELREC
00099                  PERFORM 400-WRITE-REC.
00100      SKIP3
00101  300-READ-IN-FILE.
00102      READ IN-FILE
00103        AT END
00104          MOVE 'Y' TO EOF-SW.
00105      SKIP1
00106      IF NOT EOF
00107          ADD +1 TO IN-REC-COUNT.
00108      SKIP3
00109  400-WRITE-REC.
00110      WRITE OUT-REC FROM IN-REC.
00111      SKIP1
00112      ADD +1 TO OUT-REC-COUNT.