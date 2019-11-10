00001      05 H-VOL          PIC S999             COMP-3.
00002      05 H-PROP         PIC S9(15)           COMP-3.
00003      05 H-REC-CD       PIC 9.
00004      05 H-BIRTH-DATE   PIC 9(6).
00005      05 H-NAME         PIC X(22).
00006      05 H-ADDR         PIC X(22).
00007      05 H-CITY         PIC X(12).
00008      05 FILLER         PIC X.
00009      05 H-STATE        PIC XX.
00010      05 FILLER         PIC XX.
00011      05 H-ZIP          PIC X(5).
00012      05 H-ZIPCODE REDEFINES H-ZIP PIC 9(9)  COMP-3.
00013      05 H-TXCD         PIC S9(5)            COMP-3.
00014      05 H-LBL-IND      PIC 9.
00015      05 H-YEAR-APPLD   PIC 9(4).
00016      05 H-BATCH-NO     PIC 9(5)             COMP-3.
00017      05 H-UPD-DATE     PIC 9(9)             COMP-3.
00018 *                                                95-99 MAINT. DATE
00019 *                                                      (YYYYMMDD)
00020      05 FILLER         PIC X.
