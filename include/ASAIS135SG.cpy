00001 *----------------------------------------------------------------*
00002 *             *  CERTIFICATE OF ERROR - ID SEGMENT  *
00003      05  C135-COFEID.
00004 *                                              1-40  CERTIFICATE
00005 *                                                     OF ERROR
00006 *                                                     ID SEGMENT
00007          10  C135-COFENO    PIC 9(7).
00008 *                                              1-7   KEY -
00009 *                                                     C OF E NO.
00010          10  C135-COFENO-RD  REDEFINES
00011              C135-COFENO    PIC X(7).
00012 *                                              1-7   KEY -
00013 *                                                     C OF E NO.
00014          10  C135-PROCYR    PIC 99.
00015 *                                              8-9   PROCESS YEAR
00016          10  C135-TXYR      PIC 99.
00017 *                                             10-11  TAX YEAR
00018          10  C135-TXTYP     PIC 9.
00019 *                                             12-12  TAX TYPE
00020          10  C135-ISSDTE    PIC 9(9)     COMP-3.
00021 *                                             13-17  ISSUE DATE
00022 *                                                    (0MMDDYYYY)
00023          10  C135-CTLNUM    PIC 9(5)     COMP-3.
00024 *                                             18-20  CONTROL NO.
00025          10  C135-ISSDBY    PIC X(8).
00026 *                                             21-28  ISSUED BY
00027          10  FILLER         PIC X(12).
00028 *                                             29-40  FILLER
00029 *----------------------------------------------------------------*
