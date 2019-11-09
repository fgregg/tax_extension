00001 *----------------------------------------------------------------*
00002 *             * 'O L D' - C O M P L A I N T  *
00003 *                ----- (FOR NEW DATA BASE, USE 'PICMPLAINT')
00004      05  PG-COMPLAINT.
00005 *                                              1-140 COMPLAINT
00006          10  PG-TYPE        PIC 9.
00007              88  PG-AS-TYPE         VALUE 1.
00008              88  PG-BA-TYPE         VALUE 2.
00009 *                                              1       KEY -
00010 *                                                       COMPLAINT
00011 *                                                       TYPE
00012 *                                                                *
00013          10  PG-CMPLTNO     PIC 9(7)     COMP-3.
00014 *                                              2-5   COMPLAINT NO.
00015          10  PG-REASON      PIC 9.
00016 *                                              6     REASON
00017          10  PG-MULTI       PIC 9.
00018              88  PG-NO-MULTI-COMPL  VALUE 0.
00019              88  PG-MULTIPLE-COMPL  VALUE 1.
00020 *                                              7     MULTIPLE
00021 *                                                     COMPLAINT
00022          10  PG-NAME        PIC X(22).
00023 *                                              8-29  NAME
00024          10  PG-ADDR        PIC X(22).
00025 *                                             30-51  ADDRESS
00026          10  PG-CITY        PIC X(12).
00027 *                                             52-63  CITY
00028          10  PG-STATE       PIC XX.
00029 *                                             64-65  STATE
00030          10  PG-ZIP         PIC 9(9)     COMP-3.
00031 *                                             66-70  ZIPCODE
00032          10  PG-PHONE       PIC 9(11)    COMP-3.
00033 *                                             71-76  PHONE NO.
00034          10  PG-ATTNY       PIC 9(5)     COMP-3.
00035 *                                             77-79  ATTORNEY CODE
00036 *                                                                *
00037 *                               * 80-94 VERIFICATION VALUATIONS  *
00038          10  PG-LANDVAL     PIC 9(9)     COMP-3.
00039 *                                             80-84  LAND
00040 *                                                     VALUATION
00041          10  PG-IMPRVAL     PIC 9(9)     COMP-3.
00042 *                                             85-89  IMPROVEMENT
00043 *                                                     VALUATION
00044          10  PG-TOTLVAL     PIC 9(9)     COMP-3.
00045 *                                             90-94  TOTAL
00046 *                                                     VALUATION
00047 *                                                                *
00048          10  PG-ACTION      PIC 99.
00049 *                                             95-96  ACTION
00050          10  PG-PERMIT-IND  PIC 9.
00051              88  PG-PERMIT-NO       VALUE 0.
00052              88  PG-PERMIT-YES      VALUE 1.
00053 *                                             97     PERMIT
00054 *                                                     INDICATOR
00055          10  PG-ASSMT-IND   PIC 9.
00056              88  PG-ASSMT-NO        VALUE 0.
00057              88  PG-ASSMT-YES       VALUE 1.
00058 *                                             98     ASSESSMENT
00059 *                                                     INDICATOR
00060 *                                                                *
00061 *                            *  99-140 COMPLAINT TRACKING DATES  *
00062          10  FILLER OCCURS 6 TIMES.
00063              15  PG-EMPLNO      PIC 9(5)     COMP-3.
00064 *                                             99-101 EMPLOYEE NO.
00065 *                                              ETC.
00066              15  PG-TRKDTE      PIC 9(7)     COMP-3.
00067 *                                             102-105 TRACKING
00068 *                                              ETC.    DATE
00069 *----------------------------------------------------------------*
