00001 *----------------------------------------------------------------*
00002 *           *  SENIOR FREEZE - SOCIAL SECURITY SEGMENT  *
00003      05  C155-SENFRZSOSEC.
00004 *                                              1-100 SENIOR
00005 *                                                     FREEZE
00006 *                                                     SOCIAL
00007 *                                                     SECURITY
00008 *                                                     SEGMENT
00009          10  C155-SEQ       PIC XX.
00010 *                                              1-2   KEY -
00011 *                                                     SEQUENCE
00012 *                                                     NUMBER
00013          10  C155-SSN       PIC 9(11).
00014 *                                              3-13  SOCIAL
00015 *                                                     SECURITY
00016 *                                                     NUMBER
00017          10  C155-LSTNAM    PIC X(20).
00018 *                                             14-33  LAST NAME
00019          10  C155-FSTNAM    PIC X(15).
00020 *                                             34-48  FIRST NAME
00021          10  C155-MI        PIC X.
00022 *                                             49-49  MIDDLE INIT.
00023          10  C155-TITLE     PIC XX.
00024 *                                             50-51  TITLE
00025          10  FILLER         PIC X(49).
00026 *                                             52-100 FILLER
00027 *----------------------------------------------------------------*
