00001 *                                              POS.  DESCRIPTION
00002 *
00003 *                                              1-100 VETERAN
00004 *                                                    RECORD
00005      05  VET-VOL            PIC 999.
00006 *                                              1-  3 VOLUME
00007      05  VET-REC-KEY.
00008 *                                              4- 17 RECORD KEY
00009          07  VET-PROP       PIC 9(14).
00010 *                                              4- 17 PROPERTY NO.
00011      05  VET-TWNCD          PIC 99.
00012 *                                             18- 19 TOWN CODE
00013      05  VET-REC-CD         PIC X.
00014 *                                             20- 20 RECORD CODE
00015      05  VET-NAME.
00016 *                                             21- 42 NAME
00017          10  VET-NAME-LAST      PIC X(13).
00018 *                                                 21- 33 LAST NAME
00019          10  VET-NAME-FIRST     PIC X(8).
00020 *                                                 34- 41 FIRST NAM
00021          10  VET-NAME-MI        PIC X.
00022 *                                                 42- 42 MIDDLE
00023 *                                                        INITIAL
00024      05  VET-ADDR           PIC X(22).
00025 *                                             43- 64 ADDRESS
00026      05  VET-CITY           PIC X(12).
00027 *                                             65- 76 CITY
00028      05  FILLER             PIC X.
00029 *                                             77- 77 FILLER
00030      05  VET-STATE          PIC XX.
00031 *                                             78- 79 STATE
00032      05  FILLER             PIC XX.
00033 *                                             80- 81 FILLER
00034      05  VET-ZIP            PIC X(9).
00035 *                                             82- 90 ZIP CODE
00036      05  VET-DOCKET-NO      PIC 9(9).
00037 *                                             91- 99 DOCKET NO.
00038      05  FILLER             PIC X.
00039 *                                            100-100 FILLER
