00001 *    * T A X   I N C R E M E N T   F I N A N C E   D E T A I L
00002 *                  P R O P E R T Y   M A S T E R
00003      05  TF-VOL           PIC 9(3).
00004 *                                              1-3   VOLUME
00005      05  TF-REC-KEY.
00006 *                                              4-22  RECORD KEY
00007          10  TF-PROP      PIC 9(14).
00008 *                                              4-17  PROPERTY NO.
00009          10  TF-TXTYP     PIC 9.
00010 *                                             18-18  TAX TYPE
00011          10  TF-TXYR      PIC 9(4).
00012 *                                             19-22  TAX YEAR
00013 *                                                     ADDED
00014      05  TF-TXCD          PIC 9(5).
00015 *                                             23-27  TAX CODE
00016      05  TF-AGCY          PIC 9(9).
00017 *                                             28-36  AGENCY
00018      05  TF-FRZEQVAL      PIC S9(11).
00019 *                                             37-47  FROZEN
00020 *                                                     EQUALIZED
00021 *                                                     VALUATION
00022      05  FILLER           PIC X(17).
00023 *                                             48-64  FILLER
