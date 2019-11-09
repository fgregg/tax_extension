00001 *               R A I L R O A D   S U M M A R Y
00002 *                                             POS.   DESCRIPTION
00003 *
00004 *                                             1-27   ENTIRE RECORD
00005      05  RS-VOL           PIC 9(3)  COMP-3.
00006 *                                             1- 2   VOLUME
00007      05  RS-PROP          PIC 9(15) COMP-3.
00008 *                                             3-10   PROPERTY NO.
00009      05  RS-TXCD          PIC 9(5)  COMP-3.
00010 *                                            11-13   TAX CODE
00011      05  RS-EX-RR         PIC X.
00012 *                                            14-14   EXMPT/RR
00013      05  RS-MA-CLS        PIC 9.
00014 *                                            15-15   MAJOR CLASS
00015      05  RS-MI-CLS        PIC 9(3)  COMP-3.
00016 *                                            16-17   MINOR CLASS
00017      05  RS-NBHD          PIC 9(3)  COMP-3.
00018 *                                            18-19   NEIGHBORHOOD
00019      05  RS-STRT          PIC 9(5)  COMP-3.
00020 *                                            20-22   STREET CODE
00021      05  RS-HSENO         PIC 9(5)  COMP-3.
00022 *                                            23-25   HOUSE NO.
00023      05  RS-RRNO          PIC 9(3)  COMP-3.
00024 *                                            26-27   RAILROAD NO.
00025      05  RS-ASSD-VAL      PIC 9(9)  COMP-3.
00026 *                                            28-32   ASSD. VAL.
