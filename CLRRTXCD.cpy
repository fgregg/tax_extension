00001 * ***A.CL.RTM.TAXCODYY (YY=TAX YEAR)  TAX CODE FILE DESCRIPTION***
00002 *
00003 *              O L D, USE 'TAXCODEFD' AND 'TAXCODEREC'  **
00004 *              -----       ---------       ----------
00005   02 TAXCODE-MASTER-FILE.
00006 *                                         1-207
00007 *
00008      05  TM-CODE         PIC 9(5)     COMP-3.
00009 *                                         1-3       TAX CODE
00010      05  FILLER REDEFINES TM-CODE.
00011          10  TOWN-NO        PIC X.
00012          10  FILLER         PIC XX.
00013      05  TM-RATE         PIC S9999V999  COMP-3.
00014 *                                         4-7       TAX RATE
00015      05  TM-AGENCY-CODES.
00016          10  TM-AGCY     PIC S9(9)    COMP-3 OCCURS 40 TIMES.
00017 *                                         8-12 13-17 THRU 203-207
00018 *                                                   AGENCY CODES
00019      05  TM-AGY-R REDEFINES TM-AGENCY-CODES.
00020          10  TM-AGYR     PIC X(5)  OCCURS 40 TIMES.
