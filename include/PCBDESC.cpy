00001 *----------------------------------------------------------------*
00002      05  DBD-NAME        PIC X(8).
00003      05  SEGMENT-LVL     PIC X(2).
00004          88  DB-1ST-SEG-LEVEL    VALUE '01'.
00005          88  DB-2ND-SEG-LEVEL    VALUE '02'.
00006          88  DB-3RD-SEG-LEVEL    VALUE '03'.
00007          88  DB-4TH-SEG-LEVEL    VALUE '04'.
00008          88  DB-5TH-SEG-LEVEL    VALUE '05'.
00009      05  PCB-STATUS-CODE  PIC X(2).
00010          88  GOOD-STATUS         VALUE SPACE.
00011          88  SEG-FOUND           VALUE SPACE.
00012          88  DB-END              VALUE 'GB'.
00013          88  SEG-NOTFND          VALUE 'GE'.
00014          88  DUPLICATE-SEG       VALUE 'II'.
00015          88  DUP-SEG-2NDRY-INDX  VALUE 'NI'.
00016          88  UP-LVL              VALUE 'GA'.
00017          88  NEW-SGTP-SAME-LVL   VALUE 'GK'.
00018      05  PROC-OPT        PIC X(4).
00019      05  FILLER          PIC S9(5) COMP.
00020      05  NAME-FDBK       PIC X(8).
00021      05  KEY-FDBK-LNG    PIC S9(5) COMP.
00022      05  SEN-SEG         PIC S9(5) COMP.
00023      05  KEY-FDBK.
