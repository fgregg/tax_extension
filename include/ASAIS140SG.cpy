00001 *----------------------------------------------------------------*
00002 *            *  CERTIFICATE OF ERROR - ACTION SEGMENT  *
00003      05  C140-COFEACTION.
00004 *                                              1-480 CERTIFICATE
00005 *                                                     OF ERROR
00006 *                                                     ACTION
00007 *                                                     SEGMENT
00008          10  C140-ACTNO     PIC 999.
00009 *                                              1-3   KEY -
00010 *                                                     ACTION NO.
00011          10  C140-ACTNO-RD  REDEFINES
00012              C140-ACTNO     PIC XXX.
00013 *
00014          10  C140-TYPE      PIC 99.
00015 *                                              4-5   C OF E TYPE
00016          10  C140-STAT      PIC 9.
00017 *                                              6-6   C OF E STATUS
00018 *            1 = COFE ISSUED
00019 *            2 = COFE TO BOR
00020 *            3 = COFE FROM BOR
00021 *            4 = COFE TO STATES ATTY
00022 *            5 = COFE FROM STATES ATTY
00023 *            6 = COFE TO TREASURER
00024 *            7 = COFE ADJUDICATED/CERTIFIED
00025 *            8 = COFE REFUNDED
00026 *                C140-RFNDIND = '1' --> REFUNDED
00027 *                C140-RFNDIND = '2' --> NO REFUND
00028 *                C140-RFNDIND = '3' --> BALANCE DUE
00029 *            9 = COFE VOIDED
00030 *
00031          10  C140-REASON    PIC 99.
00032 *                                              7-8   C OF E REASON
00033          10  C140-PRTIND    PIC 9.
00034 *                                              9-9   PRINT
00035 *                                                     INDICATOR
00036          10  C140-ADJIND    PIC 9.
00037 *                                             10-10  ADJUDICATED
00038 *                                                     INDICATOR
00039          10  C140-TXCD      PIC 9(5)      COMP-3.
00040 *                                             11-13  TAX CODE
00041          10  C140-TXRTE     PIC 9(6)V9(3) COMP-3.
00042 *                                             14-18  TAX RATE
00043          10  C140-AMTDUE    PIC S9(11)V99  COMP-3.
00044 *                                             19-25  AMOUNT DUE
00045          10  C140-AMTPD     PIC S9(11)V99  COMP-3.
00046 *                                             26-32  AMOUNT PAID
00047          10  C140-APPLNAME  PIC X(22).
00048 *                                             33-54  APPLICANT'S
00049 *                                                     NAME
00050          10  C140-APPLADDR  PIC X(22).
00051 *                                             55-76  APPLICANT'S
00052 *                                                     ADDRESS
00053          10  C140-CITY      PIC X(12).
00054 *                                             77-88  CITY
00055          10  C140-ST        PIC XX.
00056 *                                             89-90  STATE
00057          10  C140-ZIP       PIC 9(9)      COMP-3.
00058 *                                             91-95  ZIPCODE
00059          10  C140-APPLPHONO PIC X(11).
00060 *                                             96-106 APPLICANT'S
00061 *                                                     PHONE NO.
00062          10  C140-SECAPPLNM PIC X(22).
00063 *                                            107-128 SECONDARY
00064 *                                                     APPLICANT'S
00065 *                                                     NAME
00066          10  C140-ASSCERTVALS.
00067 *                                            129-152 ASSESSOR'S
00068 *                                                     CERTIFIED
00069 *                                                     VALUES
00070              15  C140-CASSVAL   PIC 9(13)     COMP-3.
00071 *                                            129-135 CERTIFIED
00072 *                                                     ASSESSED
00073 *                                                     VALUE
00074              15  C140-CHOEXAMT  PIC 9(13)     COMP-3.
00075 *                                            136-142 CERTIFIED
00076 *                                                     HOMEOWNER
00077 *                                                     EXEMPTION
00078 *                                                     AMOUNT
00079              15  C140-CHCOOPQTY PIC 9(5)      COMP-3.
00080 *                                            143-145 CERTIFIED
00081 *                                                     HOMESTEAD
00082 *                                                     COOP QTY
00083              15  C140-CHEXAMT   PIC 9(13)     COMP-3.
00084 *                                            146-152 CERTIFIED
00085 *                                                     HOMESTEAD
00086 *                                                     EXEMPTION
00087 *                                                     AMOUNT
00088          10  C140-ASSRECVALS.
00089 *                                            153-202 ASSESSOR'S
00090 *                                                     RECOMMENDED
00091 *                                                     VALUES
00092              15  C140-RASSVAL   PIC 9(13)     COMP-3.
00093 *                                            153-159 RECOMMENDED
00094 *                                                     ASSESSED
00095 *                                                     VALUE
00096              15  C140-RORGEQVAL PIC 9(13)     COMP-3.
00097 *                                            160-166 RECOMMENDED
00098 *                                                     ORIGINAL
00099 *                                                     EQUALIZED
00100 *                                                     VALUE
00101              15  C140-R77HOBSVAL PIC 9(13)    COMP-3.
00102 *                                            167-173 RECOMMENDED
00103 *                                                     1977
00104 *                                                     HOMEOWNER
00105 *                                                     BASE VALUE
00106              15  C140-RHOPRO    PIC 9V9(6)    COMP-3.
00107 *                                            174-177 RECOMMENDED
00108 *                                                     HOMEOWNER
00109 *                                                     PRORATION
00110              15  C140-RHOOCCFAC PIC 9(4)V9    COMP-3.
00111 *                                            178-180 RECOMMENDED
00112 *                                                     HOMEOWNER
00113 *                                                     OCCUPANCY
00114 *                                                     FACTOR
00115              15  C140-RHOCOOPQTY PIC 9(5)     COMP-3.
00116 *                                            181-183 RECOMMENDED
00117 *                                                     HOMEOWNER
00118 *                                                     COOP QTY
00119              15  C140-RHOEXAMT  PIC 9(13)     COMP-3.
00120 *                                            184-190 RECOMMENDED
00121 *                                                     HOMEOWNER
00122 *                                                     EXEMPTION
00123 *                                                     AMOUNT
00124              15  C140-RHCOOPQTY PIC 9(5)      COMP-3.
00125 *                                            191-193 RECOMMENDED
00126 *                                                     HOMESTEAD
00127 *                                                     COOP QTY
00128              15  C140-RTXAMT    PIC S9(11)V99  COMP-3.
00129 *                                            194-200 RECOMMENDED
00130 *                                                     TAX AMOUNT
00131          10  C140-ADJVALS.
00132 *                                            201-221 ADJUDICATED
00133 *                                                     VALUES
00134              15  C140-ANEWASSVAL PIC 9(13)    COMP-3.
00135 *                                            201-207 ADJUDICATED
00136 *                                                     NEW ASSESSED
00137 *                                                     VALUE
00138              15  C140-ANEWEQVAL PIC 9(13)     COMP-3.
00139 *                                            208-214 ADJUDICATED
00140 *                                                     NEW
00141 *                                                     EQUALIZED
00142 *                                                     VALUE
00143              15  C140-AADJTXAMT PIC 9(11)V99  COMP-3.
00144 *                                            215-221 ADJUDICATED
00145 *                                                     ADJUSTED
00146 *                                                     TAX AMOUNT
00147          10  C140-TOBADTE       PIC 9(9)      COMP-3.
00148 *                                            222-226 DATE TO B/A
00149 *                                                     FROM ASSR
00150 *                                                     (0MMDDYYYY)
00151          10  C140-FRBADTE       PIC 9(9)      COMP-3.
00152 *                                            227-231 DATE FROM
00153 *                                                     B/A TO ASSR
00154 *                                                     (0MMDDYYYY)
00155          10  C140-TOSADTE       PIC 9(9)      COMP-3.
00156 *                                            232-236 DATE TO S.A.
00157 *                                                     FROM ASSR
00158 *                                                     (0MMDDYYYY)
00159          10  C140-FRSADTE       PIC 9(9)      COMP-3.
00160 *                                            237-241 DATE FROM
00161 *                                                     S.A. TO ASSR
00162 *                                                     (0MMDDYYYY)
00163          10  C140-TOTRSDTE      PIC 9(9)      COMP-3.
00164 *                                            242-246 DATE TO
00165 *                                                     TREASURER
00166 *                                                     (0MMDDYYYY)
00167          10  C140-ADJDTE        PIC 9(9)      COMP-3.
00168 *                                            247-251 ADJUDICATION
00169 *                                                     DATE
00170 *                                                     (0MMDDYYYY)
00171          10  C140-AMNDMTNO      PIC 999       COMP-3.
00172 *                                            252-253 AMENDMENT
00173 *                                                     NUMBER
00174          10  C140-AMNDMTDTE     PIC 9(9)      COMP-3.
00175 *                                            254-258 AMENDMENT
00176 *                                                     DATE
00177 *                                                     (0MMDDYYYY)
00178          10  C140-UPDTDTE       PIC 9(9)      COMP-3.
00179 *                                            259-263 UPDATE DATE
00180 *                                                     (0MMDDYYYY)
00181          10  C140-UPDTID        PIC X(8).
00182 *                                            264-271 UPDATE I.D.
00183          10  C140-RFNDDTE       PIC 9(9)      COMP-3.
00184 *                                            272-276 REFUND DATE
00185 *                                                     (0MMDDYYYY)
00186          10  C140-ACCTNO        PIC 999       COMP-3.
00187 *                                            277-278 ACCOUNT NO.
00188          10  C140-DONO          PIC X(7).
00189 *                                            279-285 D & O NO.
00190          10  C140-EXVAL         PIC 9(13)     COMP-3.
00191 *                                            286-292 EXCESS
00192 *                                                     VALUATION
00193          10  C140-JRCR          PIC S9(11)V99  COMP-3.
00194 *                                            293-299 AMOUNT OF
00195 *                                                     JR CREDIT
00196          10  C140-RFND          PIC S9(11)V99  COMP-3.
00197 *                                            300-306 REFUND
00198 *                                                     AMOUNT
00199          10  C140-INTDTE        PIC 9(7)       COMP-3.
00200 *                                            307-310 INTEREST
00201 *                                                     DATE
00202 *                                                     (0MMYYYY)
00203          10  C140-INTPCT        PIC 99V9(3)    COMP-3.
00204 *                                            311-313 INTEREST
00205 *                                                     PERCENT
00206          10  C140-SOSCENO       PIC 9(9)       COMP-3.
00207 *                                            314-318 SOCIAL
00208 *                                                     SECURITY
00209 *                                                     NUMBER
00210          10  C140-WCNAME        PIC X(22).
00211 *                                            319-340 WILL CALL
00212 *                                                     NAME
00213 *
00214          10  C140-WCPHONO       PIC 9(11)      COMP-3.
00215 *                                            341-346 WILL CALL
00216 *                                                     PHONE
00217 *                                                     NUMBER
00218          10  C140-FEIN          PIC 9(9)       COMP-3.
00219 *                                            347-351 FEDERAL
00220 *                                                     I.D.
00221 *                                                     NUMBER
00222          10  C140-INTEREST-X.
00223              15  C140-INTEREST  PIC S9(9)V99    COMP-3.
00224 *                                            352-357 INTEREST
00225 *                                                     AMOUNT
00226 *
00227          10  C140-RSF-RESP      PIC X.
00228 *                                            358     RECOMMENDED
00229 *                                                    SF RESPONSE
00230          10  C140-RSF-VALUE     PIC 9(9)      COMP-3.
00231 *                                            359-363 RECOMMENDED
00232 *                                                    SF VALUE
00233          10  C140-ISS-DTE       PIC 9(9)      COMP-3.
00234 *                                            364-368 ISSUE DATE
00235 *                                                    (0MMDDYYYY)
00236          10  C140-TRCK-DATE     PIC 9(9)      COMP-3.
00237 *                                            369-373 TRACK DATE
00238 *                                                    (0MMDDYYYY)
00239          10  C140-TRCK-DISP     PIC 99.
00240 *                                            374-375 TRACKING
00241 *                                                    DISPOSITION
00242          10  C140-CSF-RESP      PIC X.
00243 *                                            376-376 CERTIFIED
00244 *                                                    SEN FREEZE
00245 *                                                    RESPONSE
00246          10  C140-CLANDVAL      PIC 9(9)      COMP-3.
00247 *                                            377-381 CERTIFIED
00248 *                                                    LAND
00249 *                                                    VALUATION
00250          10  C140-CBLDGVAL      PIC 9(9)      COMP-3.
00251 *                                            382-386 CERTIFIED
00252 *                                                    BUILDING
00253 *                                                    VALUATION
00254          10  C140-RLANDVAL      PIC 9(9)      COMP-3.
00255 *                                            387-391 RECOMMENDED
00256 *                                                    LAND
00257 *                                                    VALUATION
00258          10  C140-RBLDGVAL      PIC 9(9)      COMP-3.
00259 *                                            392-396 RECOMMENDED
00260 *                                                    BUILDING
00261 *                                                    VALUATION
00262          10  C140-CSF-VALUE     PIC 9(9)      COMP-3.
00263 *                                            397-401 CERTIFIED
00264 *                                                    SENIOR FREEZE
00265 *                                                    VALUATION
00266          10  C140-INT-DATE      PIC 9(9)      COMP-3.
00267 *                                            402-406 INTEREST
00268 *                                                    DATE
00269          10  C140-CHK-DATE      PIC 9(9)      COMP-3.
00270 *                                            407-411 CHECK
00271 *                                                    DATE
00272          10  C140-CHK-NO        PIC 9(7)      COMP-3.
00273 *                                            412-415 CHECK
00274 *                                                    NUMBER
00275          10  C140-WCADDR        PIC X(22).
00276 *                                            416-437 WILL CALL
00277 *                                                    ADDRESS
00278 *                                            FLD NOT USE EFF
00279 *                                            5/29/03
00280          10  FILLER REDEFINES C140-WCADDR.
00281              15  C140-CHOBASE   PIC 9(9)     COMP-3.
00282 *                                            416-420 CERT HOME
00283 *                                            ASSESSED VALUE
00284              15  C140-TRS-TRKDTE PIC 9(9)     COMP-3.
00285 *                                            421-425 TREASURER
00286 *                                            TRACKING DATE
00287              15  C140-CDISEXM    PIC 9(5)     COMP-3.
00288 *                                            426-428 CERTIFIED
00289 *                                               DISABLED/EXEMPTION
00290              15  C140-RDISEXM    PIC 9(5)     COMP-3.
00291 *                                            429-431 RECOMMENDED
00292 *                                               DISABLED/EXEMPTION
00293              15  FILLER         PIC X(06).
00294 *                                            432-437 FILLER
00295          10  C140-WCCITY        PIC X(12).
00296 *                                            438-449 WILL CALL CIT
00297 *                                            FLD NOT USE EFF 5/5/0
00298          10  FILLER REDEFINES C140-WCCITY.
00299              15  C140-CHOPRO      PIC 9V9(6)   COMP-3.
00300 *                                            438-441 CERTIFIED
00301 *                                                    HOMEOWNER
00302 *                                                    PRORATION
00303              15  C140-CHOCOOPQTY  PIC 9(5)   COMP-3.
00304 *                                            442-444 CERTIFIED
00305 *                                                    HOMEOWNER
00306 *                                                    COOP QTY
00307              15  C140-CHOOCCFAC   PIC 9(4)V9 COMP-3.
00308 *                                            445-447 CERTIFIED
00309 *                                                    HOMEOWNER
00310 *                                                    OCC FACTOR
00311              15  FILLER           PIC XX.
00312 *                                            448-449 FILLER
00313          10  C140-WCST          PIC XX.
00314 *                                            450-451 WILL CALL
00315 *                                                    STATE
00316 *                                            FLD NOT USE EFF
00317 *                                              05/05/2003
00318 *                                            450-450  CERT
00319 *                                                     HOME RESP
00320          10  FILLER REDEFINES C140-WCST.
00321              15  C140-CHO-RESP  PIC 9.
00322              15  C140-CHS-RESP  PIC 9.
00323 *                                            451-451  CERT HOME-
00324 *                                                     STEAD RESP
00325
00326          10  C140-WCZIP         PIC 9(9)      COMP-3.
00327 *                                            452-456 WILL CALL
00328 *                                                    ZIP CODE
00329          10  C140-RFNDIND       PIC X.
00330 *                                            457-457 REFUND
00331 *                                                     INDICATOR
00332          10  C140-HEXAMT        PIC 9(13)     COMP-3.
00333 *                                            458-464 RECOMMENDED
00334 *                                                     HOMESTEAD
00335 *                                                     EXEMPTION
00336 *                                                     AMOUNT
00337          10  C140-RHORESP       PIC X.
00338 *                                            465-465 HOMEOWNER
00339 *                                                     RESPONSE
00340          10  C140-RHSRESP       PIC X.
00341 *                                            466-466 HOMESTEAD
00342 *                                                     RESPONSE
00343          10  C140-CEQVAL        PIC 9(13)     COMP-3.
00344 *                                            467-473 CERTIFIED
00345 *                                                     EQUALIZED
00346 *                                                     VALUATION
00347          10  C140-JRNO          PIC 9(6).
00348          10  C140-JRNO-X  REDEFINES  C140-JRNO
00349                                 PIC X(6).
00350 *                                            474-479 JR NUMBER
00351          10  C140-AMNDIND       PIC X.
00352 *                                            480-480 AMENDMENT
00353 *                                                    INDICATOR
