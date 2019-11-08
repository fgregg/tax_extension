 *          DATA SET EQVAL749RD AT LEVEL 004 AS OF 06/21/06
 00010 *      --------------               BYTES     DESCRIPTION
 00020 *
 00030      03  EQU-AGENCY-NO         PIC 9(9).
 00040 *                                     1-9  AGENCY-NO.
 00050      03  EQU-AGENCY-DESC       PIC X(44).
 00060 *                                    10-53 AGENCY DESCRIPTION
 00060 *
 00061      03  EQ-AMOUNTS-VALUES.
 00062 *
 00070          05  EQU-NEW-PROP-EQLZ-VAL    PIC S9(11).
 00080 *                                    54-64 NEW PROPERTY EQUALIZE
 00090 *                                          VALUATION
 00100          05  EQU-ANNXD-PROP-EQLZ-VAL  PIC S9(11).
 00110 *                                    65-75 ANNEXED PROPERTY
 00120 *                                          EQUALIZED VALUATION
 00130          05  EQU-DISC-PROP-EQLZ-VAL   PIC S9(11).
 00140 *                                    76-86 DISCONNECTED PROPERTY
 00150 *                                          EQUALIZED VALUATION
 00160          05  EQU-RECOV-TAX-INCR-VAL   PIC S9(11).
 00170 *                                    87-97 RECOVERED TAX INCREME
 00180 *                                          VALUATION
 00190          05  EQU-EXP-INCENT           PIC S9(11).
 00200 *                                    98-108 EXPIRED INCENTIVES
