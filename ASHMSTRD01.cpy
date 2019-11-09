00001      05 SF-VOL         PIC S999             COMP-3.
00002      05 SF-PROP        PIC S9(15)           COMP-3.
00003      05 SF-REC-CD      PIC 9.
00004      05 SF-BIRTSF-DATE PIC 9(6).
00005      05 SF-NAME        PIC X(22).
00006      05 SF-ADDR        PIC X(22).
00007      05 SF-CITY        PIC X(12).
00008      05 FILLER         PIC X.
00009      05 SF-STATE       PIC XX.
00010      05 FILLER         PIC XX.
00011      05 SF-ZIP         PIC X(5).
00012      05 SF-ZIPCODE REDEFINES SF-ZIP PIC 9(9) COMP-3.
00013      05 SF-TXCD        PIC S9(5)            COMP-3.
00014      05 SF-LBL-IND     PIC 9.
00015      05 SF-YEAR-APPLD  PIC 9(4).
00016      05 SF-BATCSF-NO   PIC 9(5)             COMP-3.
00017      05 SF-SEQNO       PIC 9(9)             COMP-3.
00018      05 SF-TXTYP       PIC 9.
