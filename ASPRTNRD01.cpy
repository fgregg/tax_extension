00001 *  -- S E N I O R   F R E E Z E   P R O R A T I O N   F I L E --
00002 *                                             1-50   PRORATION-REC
00003      05  PR-PROP       PIC 9(15)  COMP-3.
00004 *                                             1-08   PROPERTY-NO.
00005      05  PR-KEYPCL     PIC 9(15)  COMP-3.
00006 *                                             9-16   KEYPARCEL NO.
00007      05  PR-RECCD      PIC 9.
00008 *                                            17-17   REC CODE
00009      05  PR-BRTHDTE    PIC 9(8).
00010 *                                            18-25   BIRTH DATE
00011      05  PR-SF-STAT    PIC X.
00012 *                                            26-26   SENIOR FREEZE
00013 *                                                    STATUS
00014      05  PR-HM-STAT    PIC X.
00015 *                                            27-27   HOMESTEAD
00016 *                                                    STATUS
00017      05  PR-HO-STAT    PIC X.
00018 *                                            28-28   HOMEOWNER
00019 *                                                    STATUS
00020      05  PR-PRORATE    PIC 9V9(6) COMP-3.
00021 *                                            29-32   PRO-RATION
00022 *                                                    FACTOR
00023      05  PR-SPLTCD     PIC X.
00024 *                                            33-33   SPLIT CODE
00025      05  FILLER        PIC X(17).
00026 *                                            34-50   FILLER
