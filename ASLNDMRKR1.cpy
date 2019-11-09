00001 *----------------------------------------------------------------*
00002 *  **  LANDMARK PROPERTIES RECORD
00003 *                                              POS.  DESCRIPTION
00004  01  LNDMRK-PROP-RECORD.
00005 *                                              1-100 BASE RECORD
00006      05  LDMK-KEY.
00007 *                                              1-18  KEY
00008          10  LDMK-PROP      PIC 9(14).
00009 *                                              1-14  PROP. NUMBER
00010          10  LDMK-YEAR      PIC 9(4).
00011 *                                             15-18  YEAR
00012      05  LDMK-TOWNNO        PIC 99.
00013 *                                             19-20  TOWN NUMBER
00014      05  LDMK-VOL           PIC 999.
00015 *                                             21-23  VOLUME
00016      05  LDMK-BASEYR        PIC 9(4).
00017 *                                             24-27  BASE YEAR
00018      05  LDMK-ASSD-VAL      PIC 9(9).
00019 *                                             28-36  ASSESSED
00020 *                                                    VALUE
00021      05  LDMK-OWNER-NME     PIC X(29).
00022 *                                             37-65  OWNER NAME
00023      05  FILLER             PIC X(35).
00024 *                                             66-100 FILLER
00025 *----------------------------------------------------------------*
