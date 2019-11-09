00001      02 V-REC1.
00002        05 V-VOL             PIC S999       COMP-3.
00003        05 V-REC-KEY.
00004          10 V-PROP-ALPHA.
00005              15 V-PROP      PIC S9(15)     COMP-3.
00006          10 V-REC-CD        PIC 9.
00007          10 V-BIRTH-DATE    PIC 9(6).
00008        05 V-NAME            PIC X(22).
00009        05 V-ADDR            PIC X(22).
00010        05 V-CITY-ST.
00011          10 V-CITY          PIC X(12).
00012          10 FILLER          PIC X.
00013          10 V-STATE         PIC XX.
00014          10 FILLER          PIC XX.
00015          10 V-ZIP           PIC X(5).
00016        05 V-TXCD            PIC S9(5)      COMP-3.
00017        05 V-LBL-IND         PIC 9.
00018        05 V-YEAR-APPLD      PIC 9(4).
00019        05 V-BATCH-NO        PIC 9(5)       COMP-3.
00020        05 V-UPD-DATE        PIC 9(9)       COMP-3.
00021 *                                           95-99 MAINT. DATE
00022 *                                                 (YYYYMMDD)
00023        05 FILLER            PIC X(1).
00024      02 V-REC2 REDEFINES V-REC1.
00025        05 V-AIX-KEY1.
00026          10 V-AIX-VOL1      PIC S999       COMP-3.
00027          10 V-AIX-PROP1     PIC S9(15)     COMP-3.
00028          10 V-AIX-REC-CD1   PIC 9.
00029          10 V-AIX-BIRTH-DATE1 PIC 9(6).
00030        05 FILLER            PIC X(83).
00031      02 V-REC3 REDEFINES V-REC1.
00032        05 FILLER            PIC X(17).
00033        05 V-AIX-KEY2.
00034          10 V-AIX-NAME2     PIC X(22).
00035          10 V-AIX-ADDR2     PIC X(22).
00036        05 FILLER            PIC X(39).
