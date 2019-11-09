00001 *----------------------------------------------------------------*
00002      05  VALIDATE-DATE2.
00003          10  VAL-MTH2     PIC 99.
00004              88  VAL-ALL-MTHS2      VALUE 01 THRU 12.
00005              88  VAL-FEB2           VALUE 02.
00006              88  VAL-4-MTHS2        VALUE 04 06 09 11.
00007              88  VAL-OTHER-MTHS2    VALUE 01 03 05 07 08 10 12.
00008          10  VAL-DAY2     PIC 99.
00009              88  VAL-ALL-DAYS2      VALUE 01 THRU 28.
00010              88  VAL-LEAP-DAYS2     VALUE 29.
00011              88  VAL-4-DAYS2        VALUE 29 30.
00012              88  VAL-OTHER-DAYS2    VALUE 29 30 31.
00013          10  VAL-FULL-YR2.
00014              15  VAL-CENT     PIC 99.
00015                  88  VAL-CENT2      VALUE     18  19 20.
00016              15  VAL-YR2      PIC 99.
00017                  88  VAL-CURR-YRS2  VALUE 00 THRU 99.
00018                  88  VAL-LEAP-YRS2  VALUE 00 04 08 12 16 20 24 28
00019                                           32 36 40 44 48 52 56 60
00020                                           64 68 72 76 80 84 88 92
00021                                           96.
00022          10  VAL-FULL-YR2-N  REDEFINES VAL-FULL-YR2
00023                               PIC 9(4).
00024      05  VALIDATE-DATE2-N REDEFINES VALIDATE-DATE2
00025                           PIC 9(8).
00026 *----------------------------------------------------------------*
