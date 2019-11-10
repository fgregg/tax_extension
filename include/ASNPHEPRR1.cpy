00001 *----------------------------------------------------------------*
00002 *  **  NPHE PRORATION FILE LAYOUT
00003 *                                              POS.  DESCRIPTION
00004  01  NP-RECORD.
00005 *                                              1-65  RECORD
00006      05  NP-KEY             PIC 9(14).
00007 *                                              1-14  KEY
00008      05  NP-KEYPCL          PIC 9(14).
00009 *                                             15-28  KEY PARCEL
00010      05  NP-STATUS-CODE     PIC X.
00011 *                                             29-29  STATUS CODE
00012      05  NP-PRIOR-AV        PIC 9(9).
00013 *                                             30-38  PRIOR ASSESSE
00014 *                                                    VALUE
00015      05  NP-CURRENT-AV      PIC 9(9).
00016 *                                             39-47  CURRENT ASSES
00017 *                                                    VALUE
00018      05  NP-TOT-PRI-AV      PIC 9(9).
00019 *                                             48-56  TOTAL PRIOR
00020 *                                                    ASSESSED VALU
00021      05  NP-TOT-CURR-AV     PIC 9(9).
00022 *                                             57-65  TOTAL CURRENT
00023 *                                                    ASSESSED VALU
00024 *----------------------------------------------------------------*
