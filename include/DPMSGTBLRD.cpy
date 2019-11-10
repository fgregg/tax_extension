00001  01  DP-TABLE-RECORD.
00002 *                                                COLS    DESC
00003      05  DT-KEY.
00004 *                                                 1-8   REC-KEY
00005          10  DT-TYPE-CODE      PIC XX.
00006 *                                                 1-2   TYPE CODE
00007          10  DT-ENTRY-CODE.
00008 *                                                 3-8   ENTRY CODE
00009              15 DT-TABLE-CODE  PIC X(4).
00010 *                                                 3-6   TABLE CODE
00011              15 DT-TC2         PIC XX.
00012 *                                                 7-8   TABLE CODE
00013      05  DT-TITLE              PIC X(60).
00014 *                                                 9-68  TITLE
