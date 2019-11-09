00001 *                                              1-75  FIXED SEGMENT
00002          05  DSEX-KEY.
00003              10  DSEX-TOWN        PIC 9(2).
00004 *                                              1-2   TOWN NUMBER
00005              10  DSEX-VOL         PIC 9(3).
00006 *                                              3-5   VOLUME NUMBER
00007              10  DSEX-PROP        PIC 9(14).
00008 *                                             6-19   PROPERTY-NO
00009          05  DSEX-RETVET-EAV  PIC 9(7)  PACKED-DECIMAL.
00010 *                                            20-23   RETURNING VET
00011 *                                                    EXEMPTION AMT
00012          05  DSEX-DISPER-EAV  PIC 9(7)  PACKED-DECIMAL.
00013 *                                            24-27   DISABLED PERS
00014 *                                                    EXEMPTION AMT
00015          05  DSEX-DISVET-EAV-1   PIC 9(7)  PACKED-DECIMAL.
00016 *                                            28-31   DISABLED VET
00017 *                                                    EXEMPTION AMT
00018 *                                                    30%-49%
00019          05  DSEX-DISVET-EAV-2   PIC 9(7)  PACKED-DECIMAL.
00020 *                                            32-35   DISABLED VET
00021 *                                                    EXEMPTION AMT
00022 *                                                    50%-69%
00023          05  DSEX-BATCH-NO       PIC 9(5)  PACKED-DECIMAL.
00024 *                                            36-38   BATCH NUMBER
00025          05  DSEX-USERID         PIC 9(7)  PACKED-DECIMAL.
00026 *                                            39-42   USERID
00027          05  DSEX-ENT-DATE       PIC 9(7)  PACKED-DECIMAL.
00028 *                                            43-46   DATE ENTERED
00029          05  DSEX-INDICATOR      PIC X(1).
00030 *                                            47-47   DISABLED
00031 *                                                      INDICATOR
00032 *           1 = RETURNING VETERAN
00033 *           2 = DISABLED PERSON
00034 **IN TAX YEAR 2015 % CHANGED FOR DISABLED VETERANS
00035 **TAX YEARS 2011 TO 2014 % WAS LESS THAN 75% & GREATER THAN 75%
00036 *           3 = DISABLED VETERAN >=30% AND <=49%
00037 *           4 = DISABLED VETERAN >=50% TO <=69%
00038 *           5 = RETURNING/DISABLED VET >=30% TO <= 49%
00039 *           6 = RETURNING/DISABLED VET >=50% TO <=69%
00040 *           7 = RETURNING VETERAN/DISABLED PERSON
00041 *           8 = DISABLED VETERAN >= 70%
00042 *
00043          05  DSEX-DISVET-EAV-3     PIC 9(7)  PACKED-DECIMAL.
00044 *                                            48-51   DISABLED VET
00045 *                                                    EXEMPTION AMT
00046 *                                                     GE 70,000
00047          05  FILLER           PIC X(24).
00048 *                                            52-75   FILLER
