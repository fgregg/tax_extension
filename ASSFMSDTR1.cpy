00001      05  OF-TOWN-NO         PIC 9(3).
00002      05  OF-VOLUME          PIC 9(3).
00003      05  OF-PROP-NO         PIC 9(14).
00004      05  OF-YEAR            PIC 9(2).
00005      05  OF-TAX-YEAR        PIC 9(2).
00006      05  OF-TAX-TYPE        PIC 9.
00007 *----------------------------------------------------------------*
00008 *               *  SENIOR FREEZE - MASTER SEGMENT  *
00009      05  OF-C145-SENFRZMASTER.
00010 *                                              1-390 SENIOR
00011 *                                                     FREEZE
00012 *                                                     MASTER
00013 *                                                     SEGMENT
00014          10  OF-C145-RECCODE PIC X.
00015 *                                              1-1   KEY -
00016 *                                                     RECORD CODE
00017 *                                                     1=NON-COOP
00018 *                                                     2=COOP
00019          10  OF-C145-MASTNAME PIC X(50).
00020 *                                              2-51  MASTER NAME
00021          10  OF-C145-MASTMAIL.
00022 *                                             52-123 MASTER
00023 *                                                     MAILING
00024 *                                                     ADDRESS
00025              15  OF-C145-HSENO PIC X(5).
00026 *                                             52-56  HOUSE NO.
00027              15  OF-C145-DIR PIC XX.
00028 *                                             57-58  DIRECTION
00029              15  OF-C145-STREET PIC X(22).
00030 *                                             59-80  STREET
00031              15  OF-C145-SUFF PIC X(4).
00032 *                                             81-84  SUFFIX
00033              15  OF-C145-CITY PIC X(28).
00034 *                                             85-112 CITY
00035              15  OF-C145-STATE PIC XX.
00036 *                                            113-114 STATE
00037              15  OF-C145-ZIP PIC 9(9).
00038 *                                            115-123 ZIP CODE
00039          10  OF-C145-BLDGUNITS PIC 9(5).
00040 *                                            124-128 BUILDING
00041 *                                                     UNITS
00042          10  OF-C145-BLDGSHARES PIC 9(6).
00043 *                                            129-134 BUILDING
00044 *                                                     SHARES
00045          10  OF-C145-PROPCT PIC 9V9(6).
00046 *                                            135-141 PRORATION
00047 *                                                     PERCENT
00048          10  OF-C145-KEYPCL PIC 9(14).
00049 *                                            142-155 KEY
00050 *                                                     PARCEL NO.
00051          10  OF-C145-SPLTCD PIC 9.
00052 *                                            156-156 SPLIT CODE
00053          10  OF-C145-OCCFAC PIC 9(4)V9.
00054 *                                            157-161 OCCUPANCY
00055 *                                                     FACTOR
00056          10  OF-C145-MAINTIND PIC 9.
00057 *                                            162-162 MAINTENANCE
00058 *                                                     INDICATOR
00059 *                                                     FOR ABOVE
00060 *                                                     4 FIELDS
00061          10  OF-C145-TERMID PIC X(4).
00062 *                                            163-166 TERMINAL I.D.
00063          10  OF-C145-UPDTID PIC X(8).
00064 *                                            167-174 UPDATE I.D.
00065          10  OF-C145-UPDTDATE PIC 9(8).
00066 *                                            175-182 UPDATE DATE
00067 *                                                     (CCYYMMDD)
00068          10  OF-C145-UPDTTIME PIC 9(7).
00069 *                                            183-189 UPDATE TIME
00070 *                                                     (0HHMMSS)
00071          10  OF-C145-288EXPAV PIC 9(9).
00072 *                                            190-198 288
00073 *                                                     EXPIRATION
00074 *                                                     ASSESSED
00075 *                                                     VALUATION
00076          10  OF-C145-288EXPEV PIC 9(9).
00077 *                                            199-207 288
00078 *                                                     EXPIRATION
00079 *                                                     EQUALIZED
00080 *                                                     VALUATION
00081          10  OF-C145-288OVRLIMAV PIC 9(9).
00082 *                                            208-216 288
00083 *                                                     OVER LIMIT
00084 *                                                     ASSESSED
00085 *                                                     VALUATION
00086          10  OF-C145-288OVRLIMEV PIC 9(9).
00087 *                                            217-225 288
00088 *                                                     OVER LIMIT
00089 *                                                     EQUALIZED
00090 *                                                     VALUATION
00091          10  OF-C145-BVYNOCALCIND PIC X.
00092 *                                            226-226 BASE VALUE
00093 *                                                     YEAR
00094 *                                                     CANNOT BE
00095 *                                                     CALCULATED
00096 *                                                     BY PROGRAM
00097 *                                                     INDICATOR
00098          10  OF-C145-BVYMANCALCIND PIC X.
00099 *                                            227-227 BASE VALUE
00100 *                                                     YEAR MANUAL
00101 *                                                     CALCULATION
00102 *                                                     INDICATOR
00103          10  OF-C145-BASVALYR PIC 9(4).
00104 *                                            228-231 BASE VALUE
00105 *                                                     YEAR
00106          10  OF-C145-BVYCLS  PIC 9(6).
00107 *                                            232-237 BASE VALUE
00108 *                                                     YEAR CLASS
00109          10  OF-C145-BVYFULLAV PIC 9(9).
00110 *                                            238-246 BASE VALUE
00111 *                                                     YEAR FULL
00112 *                                                     ASSESSED
00113 *                                                     VALUATION
00114          10  OF-C145-BVYEV   PIC 9(9).
00115 *                                            247-255 BASE VALUE
00116 *                                                     YEAR
00117 *                                                     EQUALIZED
00118 *                                                     VALUATION
00119          10  OF-C145-BVYELCOMPFULLAV PIC 9(9).
00120 *                                            256-264 BASE VALUE
00121 *                                                     YEAR ELIGIBL
00122 *                                                     COMPUTED
00123 *                                                     FULL
00124 *                                                     ASSESSED
00125 *                                                     VALUATION
00126          10  OF-C145-BVYTOTELCOMPEV PIC 9(9).
00127 *                                            265-273 BASE VALUE
00128 *                                                     YEAR TOTAL
00129 *                                                     ELIGIBLE
00130 *                                                     COMPUTED
00131 *                                                     EQUALIZED
00132 *                                                     ASSESSED
00133          10  OF-C145-CYCLS   PIC 9(6).
00134 *                                            274-279 CURRENT YEAR
00135 *                                                     CLASS
00136          10  OF-C145-CYFARMIND PIC X.
00137 *                                            280-280 CURRENT YEAR
00138 *                                                     FARM
00139 *                                                     INDICATOR
00140          10  OF-C145-CYFULLAV PIC 9(9).
00141 *                                            281-289 CURRENT YEAR
00142 *                                                     FULL
00143 *                                                     ASSESSED
00144 *                                                     VALUATION
00145          10  OF-C145-CYELCOMPAV PIC 9(9).
00146 *                                            290-298 CURRENT YEAR
00147 *                                                     ELIGIBLE
00148 *                                                     COMPUTED
00149 *                                                     ASSESSED
00150 *                                                     VALUATION
00151          10  OF-C145-CYELCOMPEV PIC 9(9).
00152 *                                            299-307 CURRENT YEAR
00153 *                                                     ELIGIBLE
00154 *                                                     COMPUTED
00155 *                                                     EQUALIZED
00156 *                                                     VALUATION
00157          10  OF-C145-CYNOTELAV PIC 9(9).
00158 *                                            308-316 CURRENT YEAR
00159 *                                                     NOT ELIGIBLE
00160 *                                                     ASSESSED
00161 *                                                     VALUATION
00162          10  OF-C145-CYNOTELEV PIC 9(9).
00163 *                                            317-325 CURRENT YEAR
00164 *                                                     NOT ELIGIBLE
00165 *                                                     EQUALIZED
00166 *                                                     VALUATION
00167          10  OF-C145-CYFNLEVDIFF PIC 9(9).
00168 *                                            326-334 CURRENT YEAR
00169 *                                                     FINAL
00170 *                                                     EQUALIZED
00171 *                                                     VALUATION
00172 *                                                     FOR TAX
00173 *                                                     COMPUTATION
00174          10  OF-C145-HOUNITS  PIC 9(5).
00175 *                                            335-339 NUMBER OF
00176 *                                                     UNITS WITH
00177 *                                                     HOMEOWNER
00178 *                                                     EXEMPTIONS
00179          10  OF-C145-HSUNITS  PIC 9(5).
00180 *                                            340-344 NUMBER OF
00181 *                                                     UNITS WITH
00182 *                                                     HOMESTEAD
00183 *                                                     EXEMPTIONS
00184          10  OF-C145-SFSHARES PIC 9(6).
00185 *                                            345-350 NUMBER OF
00186 *                                                     SHARES WITH
00187 *                                                     SENIOR FREEZ
00188          10  OF-C145-CYFULLEV PIC 9(9).
00189 *                                            351-359 CURRENT YEAR
00190 *                                                     FULL
00191 *                                                     EQUALIZED
00192 *                                                     VALUATION
00193          10  FILLER           PIC X(31).
00194 *                                            360-390
00195 *----------------------------------------------------------------*
00196 *----------------------------------------------------------------*
00197 *               *  SENIOR FREEZE - DETAIL SEGMENT  *
00198      05  OF-C150-SGSENFRZDETAIL.
00199 *                                              1-850 SENIOR
00200 *                                                     FREEZE
00201 *                                                     DETAIL
00202 *                                                     SEGMENT
00203          10  OF-C150-SGBIRTHDTE PIC X(8).
00204 *                                              1-8   KEY -
00205 *                                                     BIRTH DATE
00206 *                                                     (MMDDCCYY)
00207          10  OF-C150-SGAPPLSTNAM PIC X(20).
00208 *                                              9-28  APPLICANT
00209 *                                                     LAST NAME
00210          10  OF-C150-SGAPPFSTNAM PIC X(15).
00211 *                                             29-43  APPLICANT
00212 *                                                     FIRST NAME
00213          10  OF-C150-SGAPPMI PIC X.
00214 *                                             44-44  APPLICANT
00215 *                                                     MIDDLE INIT.
00216          10  OF-C150-SGAPPTITLE PIC XX.
00217 *                                             45-46  APPLICANT
00218 *                                                     TITLE
00219          10  OF-C150-SGAPPOLDNAM PIC X(22).
00220 *                                             47-68  APPLICANT
00221 *                                                     OLD NAME
00222          10  OF-C150-SGAPPMAIL.
00223 *                                             69-113 APPLICANT
00224 *                                                     MAILING
00225 *                                                     ADDRESS
00226              15  OF-C150-SGAPPADDR PIC X(22).
00227 *                                             69-90  ADDRESS
00228              15  OF-C150-SGAPPCITY PIC X(12).
00229 *                                             91-102 CITY
00230              15  OF-C150-SGAPPSTATE PIC XX.
00231 *                                            103-104 STATE
00232              15  OF-C150-SGAPPZIP PIC 9(9).
00233 *                                            105-113 ZIP CODE
00234          10  OF-C150-SGNAMMAINTIND PIC 9.
00235 *                                            114-114 NAME
00236 *                                                     MAINTENANCE
00237 *                                                     INDICATOR
00238 *                                                     FOR ABOVE
00239 *                                                     5 FIELDS
00240          10  OF-C150-SGSSN  PIC 9(11).
00241 *                                            115-125 SOCIAL
00242 *                                                     SECURITY
00243 *                                                     NUMBER
00244          10  OF-C150-SGAGE  PIC 999.
00245 *                                            126-128 AGE
00246          10  OF-C150-SGPHONE PIC 9(10).
00247 *                                            129-138 TELEPHONE
00248 *                                                     NUMBER
00249          10  OF-C150-SGMARSTAT PIC X.
00250 *                                            139-139 MARITAL
00251 *                                                     STATUS
00252          10  OF-C150-SGSPSREDIND PIC X.
00253 *                                            140-140 SPOUSE
00254 *                                                     RESIDENTIAL
00255 *                                                     INDICATOR
00256          10  OF-C150-SGSPSLSTNAM PIC X(20).
00257 *                                            141-160 SPOUSE
00258 *                                                     LAST NAME
00259          10  OF-C150-SGSPSFSTNAM PIC X(15).
00260 *                                            161-175 SPOUSE
00261 *                                                     FIRST NAME
00262          10  OF-C150-SGSPSMI PIC X.
00263 *                                            176-176 SPOUSE
00264 *                                                     MIDDLE INIT.
00265          10  OF-C150-SGSPSTITLE PIC XX.
00266 *                                            177-178 SPOUSE
00267 *                                                     TITLE
00268          10  OF-C150-SGSPSMAIL.
00269 *                                            179-250 SPOUSE
00270 *                                                     MAILING
00271 *                                                     ADDRESS
00272              15  OF-C150-SGSPSHSENO PIC X(5).
00273 *                                            179-183 HOUSE NO.
00274              15  OF-C150-SGSPSDIR PIC XX.
00275 *                                            184-185 DIRECTION
00276              15  OF-C150-SGSPSSTREET PIC X(22).
00277 *                                            186-207 STREET
00278              15  OF-C150-SGSPSSUFF PIC X(4).
00279 *                                            208-211 SUFFIX
00280              15  OF-C150-SGSPSCITY PIC X(28).
00281 *                                            212-239 CITY
00282              15  OF-C150-SGSPSSTATE PIC XX.
00283 *                                            240-241 STATE
00284              15  OF-C150-SGSPSZIP PIC 9(9).
00285 *                                            242-250 ZIP CODE
00286          10  FILLER         PIC X(3).
00287 *                                            251-253 FILLER
00288          10  OF-C150-SGSFSTAT PIC X.
00289 *                                            254-254 STATUS
00290          10  OF-C150-SGDENIALDTE PIC 9(8).
00291 *                                            255-262 DENIAL
00292 *                                                     DATE
00293 *                                                     (CCYYMMDD)
00294          10  OF-C150-SGFSTAPPDTE PIC 9(8).
00295 *                                            263-270 FIRST APP
00296 *                                                     RECEIVED
00297 *                                                     DATE
00298 *                                                     (CCYYMMDD)
00299          10  OF-C150-SGLSTAPPDTE PIC 9(8).
00300 *                                            271-278 LAST APP
00301 *                                                     RECEIVED
00302 *                                                     DATE
00303 *                                                     (CCYYMMDD)
00304          10  OF-C150-SGQUALDTE PIC 9(8).
00305 *                                            279-286 QUALIFIED
00306 *                                                     DATE
00307 *                                                     (CCYYMMDD)
00308          10  OF-C150-SGRTNDDTE PIC 9(8).
00309 *                                            287-294 RETURNED
00310 *                                                     DATE
00311 *                                                     (CCYYMMDD)
00312          10  OF-C150-SGMANRTNCODES.
00313 *                                            295-314 MANUAL
00314 *                                                     RETURN CODES
00315              15  OF-C150-SGMANRTNCD PIC 99 OCCURS 10 TIMES.
00316 *
00317          10  OF-C150-SGRMRKS PIC X(40).
00318 *                                            315-354 REMARKS
00319          10  OF-C150-SGMISRTNCODES.
00320 *                                            355-374 MIS
00321 *                                                     RETURN CODES
00322              15  OF-C150-SGMISRTNCD PIC 99 OCCURS 10 TIMES.
00323 *
00324          10  OF-C150-SGCOOPSENSHRS PIC 9(6).
00325 *                                            375-380 COOP
00326 *                                                     SENIR SHARES
00327          10  OF-C150-SGPCTSENSHRS PIC V9(6).
00328 *                                            381-386 PERCENT
00329 *                                                     SENIR SHARES
00330          10  OF-C150-SGLIFECARE PIC X.
00331 *                                            387-387 LIFECARE
00332 *                                                     FACILITY
00333          10  OF-C150-SGBASE-YR PIC 9(4).
00334 *                                            388-391 BASE YEAR
00335          10  OF-C150-SGBSYR-IND PIC X.
00336 *                                            392-392 BASE YEAR
00337 *                                                     INDICATOR
00338          10  OF-C150-SGBSYR-EQVAL PIC 9(9).
00339 *                                            393-401 BASE YEAR
00340 *                                                     ELIGIBLE
00341 *                                                     EQUALIZED
00342 *                                                     VALUE
00343          10  FILLER           PIC X(4).
00344 *                                            402-405 FILLER
00345          10  OF-C150-SGNOINCOME PIC X.
00346 *                                            406-406 NO INCOME
00347 *                                                     INDICATOR
00348          10  OF-C150-SGSSINC  PIC 9(7)V99.
00349 *                                            407-415 SOCIAL
00350 *                                                     SECURITY
00351 *                                                     INCOME
00352          10  OF-C150-SGRRBEN  PIC 9(7)V99.
00353 *                                            416-424 RAILROAD
00354 *                                                     BENEFITS
00355          10  OF-C150-SGCSBEN  PIC 9(7)V99.
00356 *                                            425-433 CIVIL SERVICE
00357 *                                                     BENEFITS
00358          10  OF-C150-SGOTHBEN PIC 9(7)V99.
00359 *                                            434-442 OTHER
00360 *                                                     BENEFITS
00361          10  OF-C150-SGVETBEN PIC 9(7)V99.
00362 *                                            443-451 VETERANS
00363 *                                                     BENEFITS
00364          10  OF-C150-SGPUBAID PIC 9(7)V99.
00365 *                                            452-460 PUBLIC AID
00366          10  OF-C150-SGWAGES  PIC 9(7)V99.
00367 *                                            461-469 WAGES
00368          10  OF-C150-SGINT    PIC 9(7)V99.
00369 *                                            470-478 INTEREST
00370          10  OF-C150-SGNETRENT PIC S9(7)V99.
00371 *                                            479-487 NET RENTAL
00372 *                                                     INCOME
00373          10  OF-C150-SGNETCAPGAIN PIC S9(7)V99.
00374 *                                            488-496 NET CAPITAL
00375 *                                                     GAINS
00376          10  OF-C150-SGOTHINC PIC S9(7)V99.
00377 *                                            497-505 OTHER
00378 *                                                     INCOME
00379          10  OF-C150-SGTOTINC PIC S9(7)V99.
00380 *                                            506-514 TOTAL
00381 *                                                     INCOME
00382          10  OF-C150-SGSIGNED PIC X.
00383 *                                            515-515 SIGNED
00384          10  OF-C150-SGNOTARIZED PIC X.
00385 *                                            516-516 NOTARIZED
00386          10  FILLER         PIC X(15).
00387 *                                            517-531 FILLER
00388          10  OF-C150-SGBATCH PIC 9(5).
00389 *                                            532-536 BATCH
00390          10  OF-C150-SGORIGDTE PIC 9(8).
00391 *                                            537-544 ORIGINATION
00392 *                                                     DATE
00393 *                                                     (CCYYMMDD)
00394          10  OF-C150-SGKEYTIME PIC 9(7).
00395 *                                            545-551 TIME KEYED
00396 *                                                     (0HHMMSS)
00397          10  OF-C150-SGORIGEMPNO PIC X(8).
00398 *                                            552-559 ORIGINATION
00399 *                                                     ENTRY
00400 *                                                     EMPLOYEE NO.
00401          10  OF-C150-SGTERMID PIC X(4).
00402 *                                            560-563 TERMINAL I.D.
00403          10  OF-C150-SGLSTUPID PIC X(8).
00404 *                                            564-571 LAST
00405 *                                                     UPDATE I.D.
00406          10  OF-C150-SGLSTUPDTE PIC 9(8).
00407 *                                            572-579 LAST UPDATE
00408 *                                                     DATE
00409 *                                                     (CCYYMMDD)
00410          10  OF-C150-SGLSTUPTIM PIC 9(7).
00411 *                                            580-586 LAST UPDATE
00412 *                                                     TIME
00413 *                                                     (0HHMMSS)
00414          10  FILLER         PIC X(15).
00415 *                                            587-601 FILLER
00416          10  OF-C150-SGHSSTAT PIC X.
00417 *                                            602-602 HOMESTEAD
00418 *                                                     STATUS
00419          10  OF-C150-SGSHARES PIC 9(6).
00420 *                                            603-608 SHARES
00421          10  OF-C150-SGPCTSHARES PIC 999V999.
00422 *                                            609-614 PERCENT
00423 *                                                     OF SHARES
00424          10  OF-C150-SGHSBATCH PIC 9(5).
00425 *                                            615-619 HOMESTEAD
00426 *                                                     BATCH
00427          10  OF-C150-SGHSORIGEMPNO PIC X(8).
00428 *                                            620-627 HOMESTEAD
00429 *                                                     ORIGINATION
00430 *                                                     ENTRY
00431 *                                                     EMPLOYEE NO.
00432          10  OF-C150-SGHSORIGDTE PIC 9(8).
00433 *                                            628-635 HOMESTEAD
00434 *                                                     ORIGINATION
00435 *                                                     DATE
00436 *                                                     (CCYYMMDD)
00437          10  OF-C150-SGHSKEYTIME PIC 9(7).
00438 *                                            636-642 HOMESTEAD
00439 *                                                     TIME KEYED
00440 *                                                     (0HHMMSS)
00441          10  OF-C150-SGHSTERMID PIC X(4).
00442 *                                            643-646 HOMRSTEAD
00443 *                                                     TERMINAL I.D
00444          10  OF-C150-SGHSLSTUPD PIC X(8).
00445 *                                            647-654 HOMESTEAD
00446 *                                                    LAST
00447 *                                                     UPDATE I.D.
00448          10  OF-C150-SGHSLSTUPDTE PIC 9(8).
00449 *                                            655-662 HOMESTEAD
00450 *                                                     LAST UPDATE
00451 *                                                     DATE
00452 *                                                     (CCYYMMDD)
00453          10  OF-C150-SGHSLSTUPTIM PIC 9(7).
00454 *                                            663-669 HOMESTEAD
00455 *                                                     LAST UPDATE
00456 *                                                     TIME
00457 *                                                     (0HHMMSS)
00458          10  OF-C150-SGHSYRAPPLD PIC 9(4).
00459 *                                            670-673 HOMESTEAD
00460 *                                                    YEAR APPLIED
00461          10  FILLER         PIC X(26).
00462 *                                            674-699 FILLER
00463          10  OF-C150-SGHOSTAT PIC X.
00464 *                                            700-700 HOMEOWNER
00465 *                                                     STATUS
00466          10  OF-C150-SGHOBASYR PIC 9(4).
00467 *                                            701-704 HOMEOWNER
00468 *                                                     BASE YEAR
00469          10  OF-C150-SGHOBYEQFAC PIC 9V9(4).
00470 *                                            705-709 HOMEOWNER
00471 *                                                     BASE YEAR
00472 *                                                     EQUALIZATION
00473 *                                                     FACTOR
00474          10  OF-C150-SGHOBYAV PIC 9(9).
00475 *                                            710-718 HOMEOWNER
00476 *                                                     BASE YEAR
00477 *                                                     ASSESSED
00478 *                                                     VALUATION
00479          10  OF-C150-SGHOBYEV PIC 9(9).
00480 *                                            719-727 HOMEOWNER
00481 *                                                     BASE YEAR
00482 *                                                     EQUALIZED
00483 *                                                     VALUATION
00484          10  OF-C150-SGHOTERMID PIC X(4).
00485 *                                            728-731 HOMEOWNER
00486 *                                                     TERMINAL I.D
00487          10  OF-C150-SGELGIND PIC 9.
00488 *                                            732-732 HOMEOWNER
00489 *                                                    ELIG. IND.
00490          10  FILLER         PIC XXX.
00491 *                                            733-735 FILLER
00492          10  OF-C150-SGHOLSTUPD PIC X(8).
00493 *                                            736-743 HOMEOWNER
00494 *                                                     LAST
00495 *                                                     UPDATE I.D.
00496          10  OF-C150-SGHOLSTUPDTE PIC 9(8).
00497 *                                            744-751 HOMEOWNER
00498 *                                                     LAST UPDATE
00499 *                                                     DATE
00500 *                                                     (CCYYMMDD)
00501          10  OF-C150-SGHOLSTUPTIM PIC 9(7).
00502 *                                            752-758 HOMEOWNER
00503 *                                                     LAST UPDATE
00504 *                                                     TIME
00505 *                                                     (0HHMMSS)
00506          10  OF-C150-SGMAINTIND PIC 9.
00507 *                                            759-759 MAINTENANCE
00508 *                                                     INDICATOR
00509          10  OF-C150-SGMTTERMID PIC X(4).
00510 *                                            760-763 MAINTENANCE
00511 *                                                     TERMINAL I.D
00512          10  OF-C150-SGMTUPID PIC X(8).
00513 *                                            764-771 MAINTENANCE
00514 *                                                     UPDATE I.D.
00515          10  OF-C150-SGMTUPDTE PIC 9(8).
00516 *                                            772-779 MAINTENANCE
00517 *                                                     UPDATE
00518 *                                                     DATE
00519 *                                                     (CCYYMMDD)
00520          10  OF-C150-SGMTUPTIM PIC 9(7).
00521 *                                            780-786 MAINTENANCE
00522 *                                                     UPDATE
00523 *                                                     TIME
00524 *                                                     (0HHMMSS)
00525          10  OF-C150-SFPCT     PIC 9(2).
00526          10  OF-C150-SFPCT-R REDEFINES OF-C150-SFPCT PIC 9V9.
00527 *                                            787-788 SENIOR FREEZE
00528 *                                                      PERCENT
00529          10  FILLER         PIC X(62).
00530 *                                            789-850 FILLER
00531 *----------------------------------------------------------------*
