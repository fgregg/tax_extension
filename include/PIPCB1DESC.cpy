00001 *----------------------------------------------------------------*
00002 *
00003 *
00004      05  PI-DBD-NAME       PIC X(8).
00005      05  PI-SEGMENT-LVL    PIC X(2).
00006          88  PI-DB-1ST-SEG-LEVEL VALUE '01'.
00007          88  PI-DB-2ND-SEG-LEVEL VALUE '02'.
00008          88  PI-DB-3RD-SEG-LEVEL VALUE '03'.
00009          88  PI-DB-4TH-SEG-LEVEL VALUE '04'.
00010          88  PI-DB-5TH-SEG-LEVEL VALUE '05'.
00011      05  PI-STATUS-CODE    PIC X(2).
00012          88  PI-DB-GOOD-STATUS      VALUE SPACE.
00013          88  PI-SEG-FOUND           VALUE SPACE.
00014          88  PI-DB-END              VALUE 'GB'.
00015          88  PI-SEG-NOTFND          VALUE 'GE'.
00016          88  PI-DUPLICATE-SEG       VALUE 'II'.
00017          88  PI-DUP-SEG-2NDRY-INDX  VALUE 'NI'.
00018          88  PI-UP-LVL              VALUE 'GA'.
00019          88  PI-NEW-SGTP-SAME-LVL   VALUE 'GK'.
00020      05  PI-PROC-OPT       PIC X(4).
00021      05  FILLER            PIC S9(5) COMP.
00022      05  PI-NAME-FDBK      PIC X(8).
00023      05  PI-KEY-FDBK-LNG   PIC S9(5) COMP.
00024      05  PI-SEN-SEG        PIC S9(5) COMP.
00025      05  PI-KEY-FDBK.
