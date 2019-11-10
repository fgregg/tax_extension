00001 *----------------------------------------------------------------*
00002 *                   *  R O O T   S E G M E N T  *
00003      05  P-ROOT.
00004 *                                              1-13  ROOT SEGMENT
00005          10  P-TOWN     PIC 9(3)     COMP-3.
00006              88  P-SUBURBAN-TWN VALUE 010 THRU 039.
00007              88  P-CITY-TWN     VALUE 070 THRU 077.
00008 *                                              1-2   TOWN
00009          10  P-VOL      PIC 9(3)     COMP-3.
00010              88  P-RE-VOL       VALUE 001 THRU 601.
00011              88  P-RR-VOL       VALUE 605.
00012 *                                              3-4   VOLUME
00013          10  P-KEY.
00014 *                                              5-12  ROOT KEY
00015              15  P-PROP     PIC 9(15)    COMP-3.
00016 *                                              5-12  PROPERTY-NO.
00017          10  P-STAT         PIC X.
00018              88  P-PARCEL-EXISTS    VALUE '0'.
00019              88  P-NEW-PARCEL       VALUE '1'.
00020              88  P-VOIDED-PARCEL    VALUE '2'.
00021 *                                              13    STATUS
00022 *----------------------------------------------------------------*
