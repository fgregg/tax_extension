00001 *  --  H O M E O W N E R   N P H E  ASSESSOR SEND NET FILE
00002 *                                          *BYTES* *DESCRIPTION*
00003 *                                            1-80   NPHE-REC
00004      05  PROP-NO       PIC 9(15).
00005 *                                           1-15   PROPERTY NUMBER
00006      05  BASE-YR       PIC 9(4).
00007 *                                           16-19  BASE YEAR
00008      05  NP-BASE-ASSD-VAL   PIC 9(9).
00009 *                                           20-28  BASE YEAR ASSD
00010 *                                                      VALUE
00011      05  ASMT-LVL      PIC 9(3).
00012 *                                           29-31  ASSESSMENT LEVE
00013      05  NP-BASE-ADJ-EAV   PIC 9(9).
00014 *                                           32-40  ADJ BASE YEAR
00015 *                                                   EAV VALUE
00016      05  CURR-AV       PIC 9(9).
00017 *                                           41-49  CURRENT ASSESSE
00018 *                                                      VALUE
00019      05  CURR-EAV      PIC 9(9).
00020 *                                           50-58  CURRENT EQUALIZ
00021 *                                                  ASSESSED VALUE
00022      05  NP-BASE-EAV   PIC 9(9).
00023 *                                           59-67  BASE YEAR
00024 *                                                   EAV AMOUNT
00025      05  NPHE-AMT      PIC 9(7).
00026 *                                           68-74  NPHE EXEMPTION
00027 *                                                      AMOUNT
00028      05  NPHE-CLASS    PIC 9(3).
00029 *                                           75-77  CLASS
00030      05  FILLER        PIC X(3).
